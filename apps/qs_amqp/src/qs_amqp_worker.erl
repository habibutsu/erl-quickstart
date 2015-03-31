-module(qs_amqp_worker).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(RECONNECT_INTERVAL, 500).
-define(MAX_RECONNECT_EXPONENT, 6).     % max time to next reconnect is RECONNECT_INTERVAL * 2^MAX_RECONNECT_EXPONENT

-record(state, {
    connection            :: pid(),
    connection_ref        :: reference(),
    channel               :: pid(),
    channel_ref           :: reference(), 
    params                :: #amqp_params_network{},
    connected = false     :: boolean(),
    timeout               :: non_neg_integer(),
    reconnect_attempt = 0 :: non_neg_integer()}).


start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).

init(Params) ->
    process_flag(trap_exit, true),
    %gen_server:cast(self(), {delayed_init, Params}),
    erlang:send(self(), connect),
    {ok, #state{params=Params}}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call({publish, Exchange, RoutingKey, Payload}, _From, State) ->

    Command = #'basic.publish'{exchange=Exchange, routing_key=RoutingKey},
    Message = #amqp_msg{payload=Payload},
    amqp_channel:cast(State#state.channel, Command, Message),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(connect, State) ->
    case connect(State) of
        {ok, UpdState} ->
            {noreply, UpdState};
        {error, UpdState} ->
            {noreply, reconnect(UpdState)}
    end;
handle_info({'DOWN', Ref, process, _Pid, Reason}, State)->
    lager:debug("DOWN ~p / ~p", [Ref, Reason]),
    {stop, channel_error, State};
handle_info(Info, State) ->
    lager:debug("~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:debug("reason ~p", [Reason]),
    close_connection(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


connect(State) ->
    lager:info("Connect ~p", [State#state.params]),
    case amqp_connection:start(State#state.params) of
        {ok, Connection} ->
            link(Connection),
            case amqp_connection:open_channel(Connection) of
                {ok, Channel} ->
                    ConnectionRef = erlang:monitor(process, Connection),
                    ChannelRef = erlang:monitor(process, Channel),
                    UpdState = State#state{
                        connection = Connection,
                        channel = Channel,
                        connection_ref = ConnectionRef,
                        channel_ref = ChannelRef},
                    {ok, UpdState};
                {error, Reason} ->
                    lager:error("Open channel fail: ~p", [Reason]),
                    {error, State}
            end;
        {error, Reason} ->
            lager:error("Connect fail: ~p", [Reason]),
            {error, State}
    end.

close_connection(#state{connection = Connection, channel = Channel}) ->
    case is_pid(Channel) of
        true ->
            lager:debug("Closing amqp channel ~p", [Channel]),
            amqp_channel:close(Channel);
        _ ->
            ok
    end,
    case is_pid(Connection) of
        true ->
            lager:debug("Closing amqp connection ~p", [Connection]),
            amqp_connection:close(Connection);
        _ ->
            ok
    end.

reconnect(#state{reconnect_attempt = R} = State) ->
    Pow = min(?MAX_RECONNECT_EXPONENT, R),
    T = random:uniform(erlang:round(math:pow(2, Pow)) * ?RECONNECT_INTERVAL),
    erlang:send_after(T, self(), connect),

    R1 = R + 1,
    MaxReconectInterval = erlang:round(
        math:pow(2, ?MAX_RECONNECT_EXPONENT)) * ?RECONNECT_INTERVAL,
    lager:error(
        "after ~p ms (max ~p ms), attempt ~p",
        [T, MaxReconectInterval, R1]),
    State#state{connected = false, reconnect_attempt = R1}.

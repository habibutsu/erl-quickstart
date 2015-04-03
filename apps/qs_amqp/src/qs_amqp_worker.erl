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

-define(MAX_RECONNECT_TIMEOUT, 1000*30).
-define(MIN_RECONNECT_TIMEOUT, 200).

-record(state, {
    connection            :: pid(),
    connection_ref        :: reference(),
    channel               :: pid(),
    channel_ref           :: reference(), 
    params                :: #amqp_params_network{},
    reconnect_attempt = 0 :: non_neg_integer(),
    reconnect_timeout = 0 :: non_neg_integer()}).


start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).

init(Params) ->
    process_flag(trap_exit, true),
    random:seed(now()),
    erlang:send(self(), open_connection),
    {ok, #state{params=Params}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(close_connection, _From, State) ->
    {reply, ok, close_connection(State)};
handle_call({publish, Exchange, RoutingKey, Payload}, _From, State) ->
    case {State#state.connection, State#state.channel} of
        {undefined, _} ->
            {reply, {error, no_connection}, State};
        {_, undefined} ->
            {reply, {error, no_channel}, State};
        _ ->
            Command = #'basic.publish'{
                exchange=Exchange, routing_key=RoutingKey},
            Message = #amqp_msg{payload=Payload},
            amqp_channel:call(State#state.channel, Command, Message),
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(open_connection, State) ->
    case open_connection(State) of
        {ok, UpdState} ->
            {noreply, UpdState};
        {error, UpdState} ->
            {noreply, reconnect(UpdState)}
    end;

handle_info({'DOWN', Ref, process, _Pid, Reason}, State)->
    lager:debug("DOWN ~p / ~p", [Ref, Reason]),
    {stop, channel_error, State};
handle_info(Message, State) ->
    lager:info("Info / Msg: ~p, State: ~p", [Message, State]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:debug("reason ~p", [Reason]),
    close_connection(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

open_connection(State) ->
    lager:info("Connect ~p", [State#state.params]),
    case amqp_connection:start(State#state.params) of
        {ok, Connection} ->
            erlang:link(Connection),
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

close_connection(State) ->
    UpdState = close_channel(State),
    case UpdState#state.connection of
        undefined ->
            lager:debug("Connection already closed"),
            State;
        Pid ->
            lager:debug("Closing amqp connection ~p", [Pid]),
            erlang:demonitor(UpdState#state.connection_ref),
            amqp_connection:close(Pid),
            UpdState#state{
                connection = undefined,
                connection_ref = undefined}
    end.

close_channel(State) ->
    case State#state.channel of
        undefined ->
            lager:debug("Channel already closed"),
            State;
        Pid ->
            lager:debug("Closing amqp channel ~p", [Pid]),
            erlang:demonitor(State#state.channel_ref),
            amqp_channel:close(Pid),
            State#state{
                channel = undefined,
                channel_ref = undefined}
    end.

reconnect(#state{
        reconnect_attempt = R,
        reconnect_timeout = T} = State) ->
    case T of
        ?MAX_RECONNECT_TIMEOUT ->
            reconnect_after(R, ?MIN_RECONNECT_TIMEOUT, T),
            State#state{reconnect_attempt = R + 1};
        _ ->
            T2 = min(
                exponential_backoff(R, ?MIN_RECONNECT_TIMEOUT),
                ?MAX_RECONNECT_TIMEOUT),
            reconnect_after(R, ?MIN_RECONNECT_TIMEOUT, T2),
            State#state{reconnect_attempt=R + 1, reconnect_timeout=T2}
    end.

reconnect_after(R, Tmin, Tmax) ->
    Delay = rand_range(Tmin, Tmax),
    lager:error("Reconnect after ~w ms (attempt ~w)", [Delay, R]),
    erlang:send_after(Delay, self(), open_connection).

rand_range(Min, Max) ->
    max(random:uniform(Max), Min).

exponential_backoff(N, T) ->
    erlang:round(math:pow(2, N)) * T.

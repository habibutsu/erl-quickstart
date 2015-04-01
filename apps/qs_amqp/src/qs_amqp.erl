-module(qs_amqp).

-export([
    start_pool/0,
    stop_pool/0,
    publish/3
]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(TIMEOUT, 1000).

start_pool() ->
    {ok, InitCount} = application:get_env(qs_amqp, init_count),
    {ok, MaxCount} = application:get_env(qs_amqp, max_count),

    {ok, Host} = application:get_env(qs_amqp, host),
    {ok, Port} = application:get_env(qs_amqp, port),
    {ok, Username} = application:get_env(qs_amqp, username),
    {ok, Password} = application:get_env(qs_amqp, password),
    {ok, Vhost} = application:get_env(qs_amqp, virtual_host),
    {ok, Heartbeat} = application:get_env(qs_amqp, heartbeat),
    {ok, ConnectionTimeout} = application:get_env(qs_amqp, connection_timeout),

    Params = #amqp_params_network{
        host=Host, port=Port,
        username=Username, password=Password,
        virtual_host=Vhost,
        heartbeat=Heartbeat,
        connection_timeout=ConnectionTimeout*1000},
    %Params = {amqp_params_network, Host, Port, list_to_binary(Username), list_to_binary(Password), list_to_binary(Vhost)}
    PoolConfig = [
        {name, qs_amqp_pool},
        {init_count, InitCount},
        {max_count, MaxCount},
        {start_mfa, {qs_amqp_worker, start_link, [Params]}}
    ],
    pooler:new_pool(PoolConfig).

stop_pool() ->
    % Temporary fix - https://github.com/seth/pooler/pull/45
    % TODO: change way to stop amqp pool
    Stats = pooler:pool_stats(qs_amqp_pool),
    lists:foreach(
        fun({Worker, Info}) ->
            gen_server:call(Worker, close_connection)
        end,
        Stats
    ),
    pooler:rm_pool(qs_amqp_pool).


publish(Exchange, RoutingKey, Payload) ->
    % Example
    % qs_amqp:publish(<<"amq.topic">>, <<"hello">>, <<"Hello World!">>)
    %
    case pooler:take_member(qs_amqp_pool, ?TIMEOUT) of
        Worker when is_pid(Worker) ->
            try
                gen_server:call(
                    Worker, {publish, Exchange, RoutingKey, Payload})
            catch
                Err:Reason ->
                    erlang:raise(Err, Reason, erlang:get_stacktrace())
            after
                pooler:return_member(qs_amqp_pool, Worker, ok)
            end;
        error_no_members ->
            PoolStats = pooler:pool_stats(qs_amqp_pool),
            lager:error("Pool overload: ~p", [PoolStats]),
            {error, no_members}
    end.
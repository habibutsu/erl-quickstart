-module(qs_db).

-export([
    transaction/1,
    equery/2,
    start_pool/1,
    stop_pool/0
]).

-define(TIMEOUT, 1000).
-include("qs_db.hrl").

start_pool(dummy) ->
    {ok, InitCount} = application:get_env(qs_db, init_count),
    {ok, MaxCount} = application:get_env(qs_db, max_count),

    PoolConfig = [
        {name, qs_db_pool},
        {init_count, InitCount},
        {max_count, MaxCount},
        {start_mfa, {qs_db_dummy_worker, start_link, []}}
    ],
    pooler:new_pool(PoolConfig);
start_pool(epgsql) ->
    {ok, _} = application:ensure_all_started(epgsql),

    {ok, InitCount} = application:get_env(qs_db, init_count),
    {ok, MaxCount} = application:get_env(qs_db, max_count),

    % Connection parameters
    {ok, Hots} = application:get_env(qs_db, host),
    {ok, Port} = application:get_env(qs_db, port),
    {ok, Username} = application:get_env(qs_db, username),
    {ok, Password} = application:get_env(qs_db, password),
    {ok, Database} = application:get_env(qs_db, database),
    {ok, ConnectionTimeout} = application:get_env(qs_db, connection_timeout),
    {ok, QueryTimeout} = application:get_env(qs_db, query_timeout),

    Params = #epgsql_params{
        host=Hots, port=Port,
        username=Username, password=Password,
        database=Database,
        connection_timeout=ConnectionTimeout,
        query_timeout=QueryTimeout
    },

    PoolConfig = [
        {name, qs_db_pool},
        {init_count, InitCount},
        {max_count, MaxCount},
        {start_mfa, {qs_db_epgsql_worker, start_link, [Params]}}
    ],
    pooler:new_pool(PoolConfig).

stop_pool() ->
    pooler:rm_pool(qs_db_pool).

equery(Stmt, Params) ->
    transaction(
        fun(Worker) ->
            equery({worker, Worker}, Stmt, [])
        end).

equery({worker, Worker}, Stmt, Params) ->
    gen_server:call(Worker, {equery, Stmt, Params}, infinity).

transaction(Fun) ->
    case pooler:take_member(qs_db_pool, ?TIMEOUT) of
        Worker when is_pid(Worker) ->
            W = {worker, Worker},
            try
                equery(W, "BEGIN", []),
                Result = Fun(Worker),
                equery(W, "COMMIT", []),
                Result
            catch
                Err:Reason ->
                    equery(W, "ROLLBACK", []),
                    erlang:raise(Err, Reason, erlang:get_stacktrace())
            after
                pooler:return_member(qs_db_pool, Worker, ok)
            end;
        error_no_members ->
            PoolStats = pooler:pool_stats(qs_db_pool),
            lager:error("Pool overload: ~p", [PoolStats]),
            {error, no_members}
    end.
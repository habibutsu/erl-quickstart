-module(qs_db).

-export([
    call/0,
    start_pool/0,
    stop_pool/0
]).

-define(TIMEOUT, 1000).

start_pool() ->
    {ok, InitCount} = application:get_env(qs_db, init_count),
    {ok, MaxCount} = application:get_env(qs_db, max_count),

    PoolConfig = [
        {name, qs_db_pool},
        {init_count, InitCount},
        {max_count, MaxCount},
        {start_mfa, {qs_db_dummy_worker, start_link, []}}
    ],
    pooler:new_pool(PoolConfig).

stop_pool() ->
    ok.

call() ->
    case pooler:take_member(qs_db_pool, ?TIMEOUT) of
        Worker when is_pid(Worker) ->
            try
                Worker ! hello
            catch
                Err:Reason ->
                    erlang:raise(Err, Reason, erlang:get_stacktrace())
            after
                pooler:return_member(qs_db_pool, Worker, ok)
            end;
        error_no_members ->
            PoolStats = pooler:pool_stats(qs_db_pool),
            lager:error("Pool overload: ~p", [PoolStats]),
            {error, no_members}
    end.
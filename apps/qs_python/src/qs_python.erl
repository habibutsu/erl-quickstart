-module(qs_python).

-export([
    call/3,
    start_pool/0,
    stop_pool/0
]).

-define(TIMEOUT, 1000).

call(Module, Fun, Params) ->
    case pooler:take_member(qs_python_pool, ?TIMEOUT) of
        Worker when is_pid(Worker) ->
            try
                {ok, python:call(Worker, Module, Fun, Params)}
            catch
                Err:Reason ->
                    erlang:raise(Err, Reason, erlang:get_stacktrace())
            after
                pooler:return_member(qs_python_pool, Worker, ok)
            end;
        error_no_members ->
            PoolStats = pooler:pool_stats(qs_python_pool),
            lager:error("Pool overload: ~p", [PoolStats]),
            {error, no_members}
    end.


start_pool() ->
    {ok, InitCount} = application:get_env(qs_python, init_count),
    {ok, MaxCount} = application:get_env(qs_python, max_count),

    PythonPath = filename:join(code:lib_dir(qs_python), "src_python"),
    Options = [{python_path, PythonPath}],
    PoolConfig = [
        {name, qs_python_pool},
        {init_count, InitCount},
        {max_count, MaxCount},
        {start_mfa, {python, start_link, [Options]}}
    ],
    pooler:new_pool(PoolConfig).

stop_pool() ->
    pooler:rm_pool(qs_python_pool).
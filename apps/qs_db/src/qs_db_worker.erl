-module(qs_db_worker).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init(Args) -> 
    process_flag(trap_exit, true),
    lager:debug("Init. Args: ~p", [Args]),

    spawn_link(fun() ->
        timer:sleep(120000)
    end),
    {ok, []}.

handle_call(Request, From, State) ->
    lager:info(
        "Call / Request: ~p, From: ~p, State: ~p", [Request, From, State]),
    {reply, ok, State}.

handle_cast(Message, State) ->
    lager:info("Cast / Message: ~p, State: ~p", [Message, State]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:info("Info / Msg: ~p, State: ~p", [Msg, State]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:info("Terminate / Reason: ~p, State: ~p", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
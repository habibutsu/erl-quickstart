-module(qs_python_worker).

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

-record(state, {python}).

-define(PATH_SEP, ":").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    PythonPath = filename:join(code:lib_dir(qs_python), "src_python"),
    {ok, Python} = python:start_link([{python_path, PythonPath}]),
    {ok, #state{python = Python}}.

handle_call({call, Module, Fun, Params}, _From, #state{python = Python} = State) ->
    {reply, python:call(Python, Module, Fun, Params), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{python = Python}) ->
    ok = python:stop(Python),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-module(qs_python_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),
    qs_python:start_pool(),
    {ok, self()}.

stop(_State) ->
    qs_python:stop_pool(),
    ok.

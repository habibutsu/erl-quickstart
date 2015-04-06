-module(qs_db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),
    %qs_db:start_pool(),
    qs_db:start_pool(epgsql),
    {ok, self()}.

stop(_State) ->
    qs_db:stop_pool(),
    ok.

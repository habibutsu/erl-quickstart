-module(qs_amqp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),
    qs_amqp:start_pool(),
    {ok, self()}.

stop(_State) ->
    qs_amqp:stop_pool(),
    ok.

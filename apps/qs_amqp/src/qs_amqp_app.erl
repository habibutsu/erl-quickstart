-module(qs_amqp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),
    qs_amqp_sup:start_link().

stop(_State) ->
    ok.

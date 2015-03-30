-module(qs_python_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(?MODULE, self()),
    qs_python_sup:start_link().

stop(_State) ->
    ok.

% start(_StartType, _StartArgs) ->
%     register(?MODULE, self()),
%     {ok, InitCount} = application:get_env(qs_python, init_count),
%     {ok, MaxCount} = application:get_env(qs_python, max_count),

%     PoolConfig = [
%         {name, qs_python_pool},
%         {init_count, InitCount},
%         {max_count, MaxCount},
%         {start_mfa, {qs_python_worker, start_link, []}}
%     ],
%     pooler:new_pool(PoolConfig),
%     {ok, self()}.

% stop(_State) ->
%     pooler:rm_pool(qs_python_pool),
%     ok.

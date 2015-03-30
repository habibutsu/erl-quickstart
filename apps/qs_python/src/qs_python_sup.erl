-module(qs_python_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, InitCount} = application:get_env(qs_python, init_count),
    {ok, MaxCount} = application:get_env(qs_python, max_count),

    PoolConfig = [
        {name, qs_python_pool},
        {init_count, InitCount},
        {max_count, MaxCount},
        {start_mfa, {qs_python_worker, start_link, []}}
    ],
    pooler:new_pool(PoolConfig),

    {ok, { {one_for_one, 5, 10}, []} }.


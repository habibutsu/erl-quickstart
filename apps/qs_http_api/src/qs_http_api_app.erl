-module(qs_http_api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes = [{'_', [
        {<<"/ping">>, qs_http_api_handler_ping, []}
    ]}],
    Dispatch = cowboy_router:compile(Routes),

    {ok, NumAcceptors} = application:get_env(qs_http_api, num_acceptors),
    {ok, MaxConnections} = application:get_env(qs_http_api, max_connections),
    {ok, Ip} = application:get_env(qs_http_api, ip),
    {ok, Port} = application:get_env(qs_http_api, port),
    
    {ok, ParsedIp} = inet:parse_address(Ip),

    {ok, _} = cowboy:start_http(http, NumAcceptors, [
            {ip, ParsedIp},
            {port, Port},
            {max_connections, MaxConnections}
        ],
        [{env, [{dispatch, Dispatch}]}]),

    qs_http_api_sup:start_link().

stop(_State) ->
    ok.

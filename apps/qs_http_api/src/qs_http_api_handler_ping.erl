-module(qs_http_api_handler_ping).

-behaviour(cowboy_http_handler).

-export([
    init/3,
    handle/2,
    terminate/3
]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.


handle(Req, State) ->
    Service = <<"Erlang quickstart">>,
    Applications = lists:map(
        fun({Name, _Desc, _Version}) ->
            list_to_binary(atom_to_list(Name))
        end,
        application:which_applications()),

    Reply = {[
        {<<"service">>, Service},
        {<<"applications">>, Applications}
    ]},
    qs_db:equery("select 1;", []),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, jiffy:encode(Reply), Req),
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.

%% @doc couch_npm_cache_handler.
-module(couch_npm_cache_handler).

%% API
-export([init/3, handle/2, terminate/3]).

-define(RESPONSE_HEADERS, [
    {<<"Content-Type">>, <<"application/octet-stream">>},
    {<<"access-control-allow-origin">>, <<"*">>},
    {<<"access-control-allow-methods">>, <<"GET">>},
    {<<"access-control-max-age">>, <<"86400">>}
]).

-define(IBROWSE_OPTS, [{response_format, binary}]).


-spec init({tcp | ssl, http}, cowboy_req:req(), []) -> {ok, cowboy_req:req(), []}.
init({_TransportName, http}, Req, []) ->
	{ok, Req, []}.

terminate(_Reason, _Req, _State) ->
	ok.

-spec handle(cowboy_req:req(), []) -> {ok, cowboy_req:req(), []}.
handle(Req, []) ->
    {Bindings, _} = cowboy_req:bindings(Req),
    PkgName = proplists:get_value(pkg_name, Bindings),
    TarballName = proplists:get_value(tarball_name, Bindings),
    {ok, Req2} = case request_tarball(PkgName, TarballName) of
    {error, _} ->
        Body = <<"Internal Server Error">>,
        cowboy_req:reply(500, ?RESPONSE_HEADERS, Body, Req);
    {ok, StatusCode, Body} ->
        cowboy_req:reply(StatusCode, ?RESPONSE_HEADERS, Body, Req)
    end,
    {ok, Req2, []}.


%% internal API

-spec request_tarball(binary(), binary()) -> {error, Reason::any()}
    | {ok, StatusCode::integer(), ResponseBody::binary()}.
request_tarball(PkgName, TarballName) ->
    case term_cache_ets:get(couch_npm_cache, TarballName) of
    not_found ->
        Uri = "https://registry.npmjs.org/"
            ++ binary_to_list(PkgName) ++ "/-/" ++ binary_to_list(TarballName),
        case ibrowse:send_req(Uri, [], get, <<>>, ?IBROWSE_OPTS) of
        {error, Reason} ->
            {error, Reason};
        {ok, "200", _Headers, Body} ->
            term_cache_ets:put(couch_npm_cache, TarballName, Body),
            {ok, 200, Body};
        {ok, Code, _Headers, Body} ->
            {ok, list_to_integer(Code), Body}
        end;
    {ok, Tarball} ->
        {ok, 200, Tarball}
    end.

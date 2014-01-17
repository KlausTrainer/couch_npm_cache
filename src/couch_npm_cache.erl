-module(couch_npm_cache).

%% API
-export([start/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(ibrowse),
    ok = application:start(sasl),
    ok = application:start(couchbeam),
    ok = application:start(couch_npm_cache).

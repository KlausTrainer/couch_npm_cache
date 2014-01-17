-module(couch_npm_cache_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("couch_npm_cache.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    TermCacheOpts = [
        {name, couch_npm_cache},
        {ttl, ?DEFAULT_COUCH_NPM_CACHE_TTL},
        {size, ?DEFAULT_COUCH_NPM_CACHE_SIZE}
    ],
    Processes = [
        {
            couch_npm_cache_http,
            {couch_npm_cache_http, start, [?DEFAULT_COUCH_NPM_CACHE_PORT]},
            permanent, 2000, worker, dynamic
        },
        {
            term_cache_ets,
            {term_cache_ets, start_link, [TermCacheOpts]},
            permanent, 2000, worker, dynamic
        },
        {
            couch_npm_cache_changes,
            {couch_npm_cache_changes, start_link, []},
            permanent, 2000, worker, dynamic
        }
    ],
    {ok, {{one_for_one, 5, 10}, Processes}}.

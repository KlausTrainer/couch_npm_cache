-define(DEFAULT_NPM_HOST, "isaacs.iriscouch.com").
-define(DEFAULT_NPM_PORT, 443).
-define(DEFAULT_NPM_DB_NAME, "registry").

-define(DEFAULT_COUCH_HOST, "localhost").
-define(DEFAULT_COUCH_PORT, 5984).
-define(DEFAULT_COUCH_DB_NAME, "registry").
-define(DEFAULT_COUCH_DB_OPTIONS, [{basic_auth, {"admin", "secret"}}]).

-define(DEFAULT_COUCH_NPM_CACHE_SIZE, "2GB").
-define(DEFAULT_COUCH_NPM_CACHE_TTL, 3600000).
-define(DEFAULT_COUCH_NPM_CACHE_PORT, 15984).

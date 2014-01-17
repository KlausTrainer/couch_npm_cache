PROJECT = couch_npm_cache

# options

# ERLC_OPTS := $(ERLC_OPTS)

# PLT_APPS =

# dependencies

DEPS = cowboy couchbeam
dep_cowboy = https://github.com/extend/cowboy.git 0.9.0
dep_couchbeam = https://github.com/KlausTrainer/couchbeam.git 3e23a4236300a8d88f3fc849c43f425330ac3b6b

# standard targets

include erlang.mk

check test: tests

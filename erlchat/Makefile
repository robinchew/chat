PROJECT = erlchat
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy nested sync
dep_cowboy_commit = 2.9.0
dep_nested = git https://github.com/odo/nested master

DEP_PLUGINS = cowboy # this allows templates to work with make new t=cowboy.http n=hello_handler, see cowboy/plugins.mk

BUILD_DEPS += relx
include erlang.mk

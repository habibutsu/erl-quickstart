
REBAR			= rebar
APPS 			= $(shell ls -m ./apps)
ERL_PATH		=\
	-pa $(shell pwd)/deps/*/ebin \
	-pa $(shell pwd)/apps/*/ebin

get-deps:
	${REBAR} get-deps

compile:
	${REBAR} compile

shell: get-deps compile
	erl ${ERL_PATH} -args_file etc/vm.args -config etc/app.config \
	-eval "lists:foreach(fun(A) -> {ok, _} = application:ensure_all_started(A) end, [${APPS}])."
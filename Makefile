
REBAR			= rebar
APPS 			= $(shell ls -m ./apps)
ERL_PATH		=\
	-pa $(shell pwd)/deps/*/ebin \
	-pa $(shell pwd)/apps/*/ebin

define GNUPLOT_SCRIPT
set size 1, 1
set title 'Benchmark results'
set key left top
set grid y
set xlabel 'requests'
set ylabel 'response time (ms)'
set datafile separator '\t'
plot 'result_ab.dat' every ::2 using 5 title 'response time' with lines
endef

export GNUPLOT_SCRIPT

get-deps:
	${REBAR} get-deps

compile:
	${REBAR} compile

shell: get-deps compile
	erl ${ERL_PATH} -args_file etc/vm.args -config etc/app.config \
	-eval "lists:foreach(fun(A) -> {ok, _} = application:ensure_all_started(A) end, [${APPS}])."

benchmark:
	ab -c 100 -n 10000 -g "result_ab.dat" "http://127.0.0.1:8080/ping"
	echo -e "$${GNUPLOT_SCRIPT}"| gnuplot -p

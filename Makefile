REBAR=./rebar

.PHONY: deps test

rebar:
	wget https://raw.github.com/wiki/rebar/rebar/rebar
	chmod u+x rebar

compile:
	${REBAR} compile

deps:
	${REBAR} get-deps

clean:
	${REBAR} clean

test: compile
	${REBAR} eunit

server: compile
	erl -pa ebin deps/*/ebin -eval "superchat_app:start()."

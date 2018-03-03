.PHONY: compile rel eqc test typecheck

REBAR=./rebar3

OS_NAME=$(shell uname -s)

ifeq (${OS_NAME},FreeBSD)
make="gmake"
else
MAKE="make"
endif

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test: compile
	$(REBAR) eunit

typecheck:
	$(REBAR) dialyzer


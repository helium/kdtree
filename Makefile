.PHONY: compile rel test typecheck

REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test: compile
	$(REBAR) do eunit,ct

typecheck:
	$(REBAR) dialyzer

ERL=erl
REBAR=./rebar

.PHONY: deps

all: deps compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

clean-deps:
	$(REBAR) delete-deps

run:
	./bench

console:
	ERL_LIBS=./deps:./apps $(ERL) -pa ./ebin $*

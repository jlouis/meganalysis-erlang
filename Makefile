.PHONY: all compile clean

all: compile

compile:
	rebar compile

console:
	erl -boot start_sasl -pa ./ebin

clean:
	rebar clean

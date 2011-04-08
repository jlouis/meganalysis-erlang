.PHONY: all compile

all: compile

compile:
	rebar compile

console:
	erl -pa ./ebin

# This file is part of eep0018 released under the MIT license. 
# See the LICENSE file for more information.

%.beam: %.erl
	erlc -o test/ $<

all:
	@mkdir -p ebin
	./rebar compile

check: test/etap.beam test/util.beam
	prove test/*.t

clean:
	./rebar clean
	rm test/*.beam

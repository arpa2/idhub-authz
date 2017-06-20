all: donai.beam idmap.beam donai_tests.beam diameter_arpa2_authz.beam

%.beam: %.erl
	erlc $<

clean:
	rm -rf *.beam
	#CHECKED_IN# rm -rf diameter_arpa2_authz.[eh]rl 

anew: clean all

# Generated output also checked into Git
diameter_arpa2_authz.erl diameter_arpa2_authz.hrl: diameter_arpa2_authz.dict
	diameterc $<

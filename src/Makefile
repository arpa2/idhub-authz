all: donai.beam idmap.beam xsmap.beam authz.beam 
all: routab.beam frag.beam heir.beam hash.beam
all: agent.beam
all: donai_tests.beam idmap_tests.beam xsmap_tests.beam authz_tests.beam
all: persist_sup.beam frags_sup.beam
#TO_ADD# portid.beam
#TO_ADD# diameter_arpa2_authz.beam sprocket.beam

%.beam: %.erl
	erlc $<

test: all
	./test_modules

clean:
	rm -rf *.beam
	#CHECKED_IN# rm -rf diameter_arpa2_authz.[eh]rl 

anew: clean all test

# Generated output also checked into Git
diameter_arpa2_authz.erl diameter_arpa2_authz.hrl: diameter_arpa2_authz.dict
	diameterc $<

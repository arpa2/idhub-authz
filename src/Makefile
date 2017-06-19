all: donai.beam idmap.beam donai_tests.beam

%.beam: %.erl
	erlc $<

clean:
	rm -rf *.beam

anew: clean all


diameter.erl diameter.hrl: diameter.dict
	diameterc $<

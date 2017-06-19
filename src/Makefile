all: donai.beam idmap.beam

%.beam: %.erl
	erlc $<

clean:
	rm -rf *.beam

anew: clean all

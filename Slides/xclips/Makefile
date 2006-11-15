all: $(patsubst %.slides,%.xul,$(wildcard *.slides))

%.xul: %.slides template/*
	perl bin/render-template $< $@

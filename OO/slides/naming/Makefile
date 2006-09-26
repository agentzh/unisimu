all: naming.xul

%.xul: %.slides template/*
	perl bin/render-template $< $@

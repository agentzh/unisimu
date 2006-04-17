all: patterns.xul patterns2.xul

%.xul: %.slides template/*
	perl bin/render-template $< $@

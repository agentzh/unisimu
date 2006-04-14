all: patterns.xul

patterns.xul: patterns.slides template/*
	perl bin/render-template $< $@

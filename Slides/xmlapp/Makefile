all: xmlapp.xul

%.xul: %.slides template/*
	perl bin/render-template $< $@

all: naming.xul naming_recap.xul

%.xul: %.slides template/*
	perl bin/render-template $< $@

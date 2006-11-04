all: func.xul

%.xul: %.slides template/*
	perl bin/render-template $< $@

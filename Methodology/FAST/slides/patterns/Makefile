all: patterns.xul

patterns.xul: patterns.slides template/*
	perl -S render-template $< $@

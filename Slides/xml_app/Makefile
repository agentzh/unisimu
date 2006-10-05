all: xml_app.xul

%.xul: %.slides template/*
	perl bin/render-template $< $@

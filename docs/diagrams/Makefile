SHELL := /usr/bin/env bash

.PHONY: diagrams clean_diagrams

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available commands:"
	@echo "  diagrams       -- Create the diagrams from .dot files"
	@echo "  clean_diagrams -- Delete the diagram images"

DOT_INPUTS := $(wildcard ./*.dot)
DOT_SVGS := $(patsubst %.dot,%.svg,$(DOT_INPUTS))
DOT_PNGS := $(patsubst %.dot,%.png,$(DOT_INPUTS))

diagrams: $(DOT_SVGS) $(DOT_PNGS)
	@echo $(DOT_SVGS)

clean_diagrams:
	rm $(DOT_SVGS)
	rm $(DOT_PNGS)

%.png: %.svg
	convert $< $@

%.svg: %.dot
	dot -Tsvg $< -o $@

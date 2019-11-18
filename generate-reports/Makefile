#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# squareland/generate-reports/Makefile

HERE := $(shell git rev-parse --show-toplevel)
TRUTH := $(HERE)/generate-ground-truth/output/truth.feather

.PHONY: all clean

all: output/reports.feather

clean: 
		-rm -r output/*

output/reports.feather: \
		src/observe.R \
		$(TRUTH)
	Rscript --vanilla $< \
			--input=$(TRUTH) \
			--seed=202070170 \
			--nsource=4 \
			--reports=$@

# done.

#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# /Users/tshah/git/squareland/generate-ground-truth/Makefile

.PHONY: all clean

all: output/truth.feather output/truth.png

clean: 
		-rm -r output/*

output/truth.feather: \
		src/generate-truth.R
	Rscript --vanilla $< \
			--seed=19481210 \
			--n_centers=7 \
			--mean_incidents=1000 \
			--output=$@

output/truth.png: \
		src/plot-truth.R \
		output/truth.feather
	Rscript --vanilla $< \
			--input=output/truth.feather \
			--output=$@

# done

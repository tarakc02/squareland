#
# Authors:     TS
# Maintainers: TS
# Copyright:   2019, HRDAG, GPL v2 or later
# =========================================
# squareland/mse/Makefile

HERE := $(shell git rev-parse --show-toplevel)
REPORTS := $(HERE)/generate-reports/output/reports.feather

.PHONY: all clean

all: \
	output/cell-counts.feather \
	output/model1.rds \
	output/model2.rds \
	output/model3.rds \
	output/cell-counts-region.feather \
	output/model-3-regions.png \
	output/model-1-ppc-docpattern.png \
	output/model-1-ppc-gridcell.png \
	output/model-2-ppc-docpattern.png \
	output/model-2-ppc-gridcell.png \
	output/model-3-ppc-docpattern.png \
	output/model-3-ppc-gridcell.png

clean: 
		-rm -r output/*

output/cell-counts.feather: \
		src/summarize_reports.R \
		$(REPORTS)
	Rscript --vanilla $< \
			--input=$(REPORTS) \
			--output=$@

output/model1.rds: \
		src/model1.R \
		output/cell-counts.feather
	Rscript --vanilla $< \
			--input=output/cell-counts.feather \
			--seed=971398671 \
			--output=$@

output/model2.rds: \
		src/model2.R \
		output/cell-counts.feather
	Rscript --vanilla $< \
			--input=output/cell-counts.feather \
			--seed=370712 \
			--output=$@

output/model-1-ppc-docpattern.png output/model-1-ppc-gridcell.png: \
		src/ppc.R \
		output/cell-counts.feather \
		output/model1.rds
	Rscript --vanilla $< \
			--data=output/cell-counts.feather \
			--model=output/model1.rds \
			--modelname=model_1 \
			--outputdir=output

output/model-2-ppc-docpattern.png output/model-2-ppc-gridcell.png: \
		src/ppc.R \
		output/cell-counts.feather \
		output/model2.rds
	Rscript --vanilla $< \
			--data=output/cell-counts.feather \
			--model=output/model2.rds \
			--modelname=model_2 \
			--outputdir=output

output/model-3-ppc-docpattern.png output/model-3-ppc-gridcell.png: \
		src/ppc.R \
		output/cell-counts-region.feather \
		output/model3.rds
	Rscript --vanilla $< \
			--data=output/cell-counts-region.feather \
			--model=output/model3.rds \
			--modelname=model_3 \
			--outputdir=output

# done.

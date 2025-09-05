.PHONY: setup analysis clean

setup:
	Rscript analysis/setup.R

analysis:
	Rscript analysis/run_analysis.R

clean:
	rm -rf analysis/outputs/* data/processed/*

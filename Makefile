.PHONY: all setup analysis clean help

# Default target: show help
all: help

help:
	@echo "═══════════════════════════════════════════════════════════"
	@echo "  Saliency Project - Analysis Pipeline"
	@echo "═══════════════════════════════════════════════════════════"
	@echo ""
	@echo "Available targets:"
	@echo "  make help       - Show this help message"
	@echo "  make setup      - Initialize R environment (non-Docker users only)"
	@echo "  make analysis   - Run the full analysis pipeline"
	@echo "  make clean      - Remove generated outputs (keeps raw data)"
	@echo ""
	@echo "Quick Start:"
	@echo "  Docker users:   make analysis"
	@echo "  Local R users:  make setup && make analysis"
	@echo ""
	@echo "Direct R usage:"
	@echo "  Rscript analysis/setup.R         # One-time setup"
	@echo "  Rscript analysis/run_analysis.R  # Run analysis"
	@echo ""

setup:
	@echo "Running setup (for non-Docker users)..."
	@Rscript analysis/setup.R
	@echo ""

analysis:
	@echo "Running analysis pipeline..."
	@Rscript analysis/run_analysis.R
	@echo ""
	@echo "✓ Analysis complete! Check analysis/outputs/ for results."

clean:
	@echo "Cleaning generated outputs..."
	@rm -rf analysis/outputs/figs/* analysis/outputs/tables/*
	@rm -f data/processed/*
	@echo "✓ Cleaned. Raw data and code preserved."

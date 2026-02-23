.PHONY: help build run install clean

CABAL ?= cabal
EXE ?= setup-cli
BIN_DIR ?= .bin

help:
	@echo "Targets:"
	@echo "  make build            Build $(EXE)"
	@echo "  make run ARGS='...'   Run $(EXE) with args"
	@echo "  make install          Install $(EXE) to $(BIN_DIR)/"
	@echo "  make clean            Remove build artifacts"

build:
	$(CABAL) build exe:$(EXE)

run:
	$(CABAL) run $(EXE) -- $(ARGS)

install:
	mkdir -p $(BIN_DIR)
	$(CABAL) install exe:$(EXE) \
		--installdir=$(PWD)/$(BIN_DIR) \
		--install-method=copy \
		--overwrite-policy=always

clean:
	rm -rf dist-newstyle

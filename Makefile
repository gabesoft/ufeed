TIX := $(shell find . -name "*.tix")
BIN := $(CURDIR)/result/bin
UFEED_PORT = 8008

API_SERVER_KAPI_XA = http://localhost:8001/xandar
API_SERVER_KAPI_LO = http://localhost:8001/lono

SERVER_ARGS_KAPI_XA = "$(UFEED_PORT)" "$(API_SERVER_KAPI_XA)" +RTS -N
UPDATE_ARGS_KAPI_XA =  "$(API_SERVER_KAPI_XA)" +RTS -N

SERVER_ARGS_KAPI_LO = "$(UFEED_PORT)" "$(API_SERVER_KAPI_LO)" +RTS -N
UPDATE_ARGS_KAPI_LO =  "$(API_SERVER_KAPI_LO)" +RTS -N

# Run the ghci repl
repl:
	stack ghci --main-is ufeed:exe:ufeed-server

# Run the gchi repl (set NIX_PATH first)
repl-nix: export NIX_PATH=$(HOME)/.nix-defexpr/channels
repl-nix: repl

# Start a nix-shell
nix-shell: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-shell:
	nix-shell -p zlib bzip2

# Nix build and output to /result/bin
nix-build: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-build:
	nix-build
	ls -lA ./result/bin

# Package for Nix
nix-package: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-package:
	cabal2nix . > ufeed.nix

# Setup stack
setup:
	stack setup

# Build the project
build: setup
	stack build

# Build and install
install:
	stack build --copy-bins

# Build and watch for file changes
watch:
	stack build --file-watch

# Run tests and watch for file changes
test-watch:
	stack build --file-watch --test

# Run tests
test:
	stack build --test

# Run a single test
#   sample call: make test-only test=funct-tests
test-only:
	stack build --test hapro:$$test

# Run the updater for the Xandar app
update-xandar: build
	stack exec ufeed-updater -- $(UPDATE_ARGS_KAPI_XA)

# Run the updater for the Lono app
update-lono: build
	stack exec ufeed-updater -- $(UPDATE_ARGS_KAPI_LO)

# Run the server for the Xandar app
serve-xandar: export RUN_ENV = development
serve-xandar: build
	stack exec ufeed-server -- $(SERVER_ARGS_KAPI_XA)

# Run the server for the Lono app
serve-lono: export RUN_ENV = development
serve-lono: build
	stack exec ufeed-server -- $(SERVER_ARGS_KAPI_LO)

# Run the server for the Xandar app
nix-serve-xandar: export RUN_ENV = development
nix-serve-xandar: nix-build
	$(BIN)/ufeed-server $(SERVER_ARGS_KAPI_XA)

# Run the server for the Lono app
nix-serve-lono: export RUN_ENV = development
nix-serve-lono: nix-build
	$(BIN)/ufeed-server $(SERVER_ARGS_KAPI_LO)

.PHONY: release test loc clean update serve serve-dask watch build

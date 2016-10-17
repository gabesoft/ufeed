TIX := $(shell find . -name "*.tix")
BIN := $(CURDIR)/result/bin
API_SERVER = http://localhost:8006
SERVER_ARGS = 8008 "$(API_SERVER)" +RTS -N
UPDATE_ARGS =  "$(API_SERVER)" +RTS -N

repl:
	stack ghci --ghc-options "-package ghci-pretty"

repl-nix: export NIX_PATH=$(HOME)/.nix-defexpr/channels
repl-nix: repl

nix-shell: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-shell:
	nix-shell -p zlib bzip2

nix-build: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-build:
	nix-build
	ls -lA ./result/bin

nix-gen: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-gen:
	cabal2nix . > ufeed.nix

setup:
	stack setup

build: setup
	stack build

install:
	stack build --copy-bins

watch:
	stack build --file-watch

test-watch:
	stack build --file-watch --test

test:
	stack build --test

# sample call: make test-only test=funct-tests
test-only:
	stack build --test hapro:$$test

update: build
	stack exec ufeed-updater -- $(UPDATE_ARGS)

serve: export RUN_ENV = development
serve: build
	stack exec ufeed-server -- $(SERVER_ARGS)

nix-serve: export RUN_ENV = development
nix-serve: nix-build
	$(BIN)/ufeed-server $(SERVER_ARGS)

.PHONY: release test loc clean

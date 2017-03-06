TIX := $(shell find . -name "*.tix")
BIN := $(CURDIR)/result/bin
UFEED_PORT = 8008

API_SERVER = http://localhost:8006
API_SERVER_KAPI = http://localhost:8001

SERVER_ARGS = "$(UFEED_PORT)" "$(API_SERVER)" +RTS -N
UPDATE_ARGS =  "$(API_SERVER)" +RTS -N

SERVER_ARGS_KAPI = "$(UFEED_PORT)" "$(API_SERVER_KAPI)" +RTS -N
UPDATE_ARGS_KAPI =  "$(API_SERVER_KAPI)" +RTS -N

repl:
	stack ghci --main-is ufeed:exe:ufeed-server

repl-nix: export NIX_PATH=$(HOME)/.nix-defexpr/channels
repl-nix: repl

nix-shell: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-shell:
	nix-shell -p zlib bzip2

nix-build: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-build:
	nix-build
	ls -lA ./result/bin

nix-package: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-package:
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

update-kapi: build
	stack exec ufeed-updater -- $(UPDATE_ARGS_KAPI)

serve: export RUN_ENV = development
serve: build
	stack exec ufeed-server -- $(SERVER_ARGS)

serve-kapi: export RUN_ENV = development
serve-kapi: build
	stack exec ufeed-server -- $(SERVER_ARGS_KAPI)

nix-serve: export RUN_ENV = development
nix-serve: nix-build
	$(BIN)/ufeed-server $(SERVER_ARGS)

.PHONY: release test loc clean

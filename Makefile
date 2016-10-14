
TIX := $(shell find . -name "*.tix")

repl:
	stack ghci --ghc-options "-package ghci-pretty"

repl-nix: export NIX_PATH=$(HOME)/.nix-defexpr/channels
repl-nix: repl

nix-shell: export NIX_PATH=$(HOME)/.nix-defexpr/channels
nix-shell:
	nix-shell -p zlib bzip2

setup:
	stack setup

build-nix: export NIX_PATH=$(HOME)/.nix-defexpr/channels
build-nix:
	nix-build
	ls -lA ./result/bin

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
	stack exec ufeed-updater -- "http://localhost:8006" +RTS -N

serve: export RUN_ENV = development
serve: build
	stack exec ufeed-server -- 8008 "http://localhost:8006" +RTS -N

.PHONY: release test loc clean

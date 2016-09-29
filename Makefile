
TIX := $(shell find . -name "*.tix")

repl:
	stack ghci --ghc-options "-package ghci-pretty"

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
	stack exec ufeed-updater -- "http://localhost:8006" +RTS -N

serve: export RUN_ENV = development
serve: build
	stack exec ufeed-server -- 8008 "http://localhost:8006" +RTS -N

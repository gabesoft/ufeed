
TIX := $(shell find . -name "*.tix")

repl:
	stack ghci --ghc-options "-package ghci-pretty"

clean-tix:
	@$(RM) $(TIX)

build: clean-tix
	stack build

test: clean-tix
	stack build --test

# sample call: make test-only test=funct-tests
test-only: clean-tix
	stack build --test hapro:$$test

update: build
	stack exec updater -- "http://localhost:8006" +RTS -N

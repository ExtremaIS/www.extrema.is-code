##############################################################################
# Project configuration

DATA_PATH := output.txt

##############################################################################
# Make configuration

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error GNU Make 4.0 or later required)
endif
.RECIPEPREFIX := >

SHELL := bash
.SHELLFLAGS := -o nounset -o errexit -o pipefail -c

MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --warn-undefined-variables

.DEFAULT_GOAL := benchmarks

##############################################################################
# Functions

define die
  (echo "error: $(1)" ; false)
endef

define hs_files
  find . -not -path '*/\.*' -type f -name '*.hs'
endef

##############################################################################
# Rules

bench-bs: build
bench-bs: # run bench-bs benchmark
> stack exec bench-bs
.PHONY: bench-bs

bench-bslines: build
bench-bslines: # run bench-bslines benchmark
> stack exec bench-bslines
.PHONY: bench-bslines

bench-cbs: build
bench-cbs: # run bench-cbs benchmark
> stack exec bench-cbs
.PHONY: bench-cbs

bench-cbslines: build
bench-cbslines: # run bench-cbslines benchmark
> stack exec bench-cbslines
.PHONY: bench-cbslines

bench-text: build
bench-text: # run bench-text benchmark
> stack exec bench-text
.PHONY: bench-text

benchmarks: bench-text
benchmarks: bench-bs
benchmarks: bench-bslines
benchmarks: bench-cbs
benchmarks: bench-cbslines
benchmarks: # run all benchmarks
.PHONY: benchmarks

build: hr
build: # build package
> @stack build
.PHONY: build

clean: # clean package
> @stack clean
.PHONY: clean

clean-all: clean # clean package and remove artifacts
> @rm -rf .stack-work
> @rm -f *.yaml.lock
> @rm -f $(DATA_PATH)
.PHONY: clean-all

help: # show this help
> @grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
>   | sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
>   | column -t -s $$'\t'
.PHONY: help

hlint: # run hlint on all Haskell source
> @$(call hs_files) | xargs hlint
.PHONY: hlint

hr: #internal# display a horizontal rule
> @command -v hr >/dev/null 2>&1 && hr -t || true
.PHONY: hr

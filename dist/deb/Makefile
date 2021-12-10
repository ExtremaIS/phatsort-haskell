DESTDIR     ?=
prefix      ?= /usr
bindir      ?= $(DESTDIR)/$(prefix)/bin
datarootdir ?= $(DESTDIR)/$(prefix)/share
docdir      ?= $(datarootdir)/doc/phatsort-haskell
man1dir     ?= $(datarootdir)/man/man1

SHELL := bash

build:
	@stack build
.PHONY: build

install:
	$(eval LIROOT := $(shell stack path --local-install-root))
	@mkdir -p "$(bindir)"
	@install -m 0755 "$(LIROOT)/bin/phatsort" "$(bindir)/phatsort"
	@install -m 0755 "$(LIROOT)/bin/seqcp" "$(bindir)/seqcp"
	@mkdir -p "$(man1dir)"
	@install -m 0644 -T <(gzip -c doc/phatsort.1) "$(man1dir)/phatsort.1.gz"
	@install -m 0644 -T <(gzip -c doc/seqcp.1) "$(man1dir)/seqcp.1.gz"
	@mkdir -p "$(docdir)"
	@install -m 0644 -T <(gzip -c README.md) "$(docdir)/README.md.gz"
	@install -m 0644 -T <(gzip -c CHANGELOG.md) "$(docdir)/changelog.gz"
	@install -m 0644 -T <(gzip -c LICENSE) "$(docdir)/LICENSE.gz"
.PHONY: install

test:
	@stack test
.PHONY: test

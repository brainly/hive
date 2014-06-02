# Some Rebar related env:
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
REBAR=./rebar

# The default configuration file:
PLUGINS=plugins/
SCHEMA=etc/schema/
CONFIG=etc/hive.json

all: build

build:
	@$(REBAR) get-deps compile

run:
	@escript priv/test_config.erl $(CONFIG)
	@sh priv/start.sh $(PLUGINS) $(SCHEMA) $(CONFIG)

run-dev:
	@echo $(CONFIG)
	@escript priv/test_config.erl $(CONFIG)
	@sh priv/start-dev.sh $(PLUGINS) $(SCHEMA) $(CONFIG)

unit-test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

test-config:
	@escript priv/test_config.erl $(CONFIG)

rev:
	@sh priv/make_revision_tex.sh docs/revision.tex

deb-package: deb-changelog

deb-changelog:
	@touch debian/changelog
	@sh priv/make_changelog.sh > debian/changelog

.PHONY: clean
clean:
	@$(REBAR) clean

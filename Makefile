# Some Rebar related env:
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
REBAR=./rebar

# The default configuration file:
PLUGINS=plugins/
SCHEMA=etc/schema/
CONFIG=etc/hive.json

TARGET=binary
BUILD=""

all: build

build: version
	@$(REBAR) get-deps compile

run:
	@escript priv/test_config.erl $(PLUGINS) $(SCHEMA) $(CONFIG)
	@sh priv/start.sh $(PLUGINS) $(SCHEMA) $(CONFIG)

run-dev:
	@echo $(CONFIG)
	@escript priv/test_config.erl $(PLUGINS) $(SCHEMA) $(CONFIG)
	@sh priv/start-dev.sh $(PLUGINS) $(SCHEMA) $(CONFIG)

unit-test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

test-config:
	@escript priv/test_config.erl $(PLUGINS) $(SCHEMA) $(CONFIG)

version:
	@touch docs/revision.tex
	@touch include/hive_version.hrl
	@sh priv/make_version_files.sh docs/revision.tex include/hive_version.hrl

deb-package: deb-changelog deb-control deb-install
	@debuild $(TARGET)

deb-changelog:
	@touch debian/changelog
	@sh priv/make_changelog.sh $(BUILD) > debian/changelog

deb-control:
	@touch debian/control
	@sh priv/make_control.sh $(BUILD) > debian/control

deb-install:
	@touch debian/install
	@sh priv/make_install.sh > debian/install

.PHONY: clean
clean:
	@$(REBAR) clean

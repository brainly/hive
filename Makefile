# Some Rebar related env:
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
REBAR=./rebar

# The default configuration file:
CONFIG=etc/hive.json

all: build

build:
	@$(REBAR) get-deps compile

run:
	@escript priv/test_config.erl $(CONFIG)
	@sh priv/start.sh $(CONFIG)

run-dev:
	@echo $(CONFIG)
	@escript priv/test_config.erl $(CONFIG)
	@sh priv/start-dev.sh $(CONFIG)

unit-test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

test-config:
	@escript priv/test_config.erl $(CONFIG)

rev:
	@sh priv/make_revision_tex.sh docs/revision.tex

clean:
	@$(REBAR) clean

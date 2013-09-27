#! /bin/sh
exec erl +K true \
    $(escript priv/prep_hive.erl $@) \
    -pa ebin edit deps/*/ebin \
    -sname hive_dev \
    -s lager \
    -s hive start development $@


#! /bin/sh
exec erl +K true \
    $(escript priv/prep_hive.erl $@) \
    -pa ebin edit deps/*/ebin \
    -s lager \
    -s hive start_dev $@


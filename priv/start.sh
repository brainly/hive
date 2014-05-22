#!/bin/sh
exec erl +K true \
     $(escript priv/prep_hive.erl $@) \
    -noinput \
    -pa ebin edit deps/*/ebin \
    -sname hive \
    -s lager \
    -s hive start $@


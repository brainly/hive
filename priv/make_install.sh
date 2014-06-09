#!/bin/sh

echo "ebin /opt/hive"
echo "etc /opt/hive"
echo "priv /opt/hive"
echo "plugins /opt/hive"
echo "Makefile /opt/hive"
echo "rebar /opt/hive"
echo "rebar.config /opt/hive"

for DEPS in $(ls deps); do
    echo "deps/$DEPS/ebin /opt/hive/deps/$DEPS"

    if [ -d  "deps/$DEPS/priv" ]; then
       echo "deps/$DEPS/priv /opt/hive/deps/$DEPS" 
    fi

    if [ -d  "deps/$DEPS/etc" ]; then
       echo "deps/$DEPS/etc /opt/hive/deps/$DEPS" 
    fi
done

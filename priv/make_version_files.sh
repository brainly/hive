#! /bin/sh

TAG=$(git tag | tail -n1)

# Revision TeX:
echo "$TAG" > $1

# Hive version:
echo "-define(HIVE_VERSION, <<\"$TAG\">>)." > $2

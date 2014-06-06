#!/bin/sh

echo "Source: brainly-hive"
echo "Section: unknown"
echo "Priority: extra"
echo "Maintainer: $(git log -n1 --pretty=format:'%an <%ae>')"
echo "Build-Depends: debhelper (>= 7.0.50~)"
echo "Standards-Version: 3.8.4"
echo 
echo "Package: brainly-hive"
echo "Architecture: all"
echo 'Depends: ${shlibs:Depends}, ${misc:Depends}, esl-erlang (>= 1:16.b)'
echo "Description: Hive $(git tag | tail -n1)"

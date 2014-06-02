#!/bin/sh

echo "Source: hive"
echo "Section: unknown"
echo "Priority: extra"
echo "Maintainer: $(git log -n1 --pretty=format:'%an <%ae>')"
echo "Build-Depends: debhelper (>= 7.0.50~)"
echo "Standards-Version: 3.8.4"
echo 
echo "Package: hive"
echo "Architecture: all"
echo "Depends: ${schlibs:Depends}, ${misc:Depends}"
echo "Description: Hive $(git tag | tail -n1)"
#!/bin/bash

# Clean out existing coverage data, then instrument and re-run unit tests and produce new coverage data.
# There is also a little custom cleanup performed on the xml file to make it work better with VSCode.
# At the end it cleans out instrumented code and rebuilds with non-instrumented to prevent IDE confusion.

set -x

find . -name cobertura.xml -exec rm -rf {} \;

sbt clean coverage test
sbt coverageReport

for a in $(find . -name cobertura.xml)
  do
  echo Correcting coverage file $a
  sed -i '' -e '/<source>--source<\/source>/d' $a
  pushd `dirname $a`
  mkdir -p ../../../latestCoverage
  cp cobertura.xml ../../../latestCoverage
  popd
  done

# rm -rf latestCoverage
# mkdir -p latestCoverage
# sbt coverageAggregate

cp -af target/scala-3.2.2/scoverage-report/* latestCoverage

sbt clean Test/compile


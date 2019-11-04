#!/bin/bash

SCIPT_DIR="${0%/*}"
PROJECT_DIR="$SCIPT_DIR/.."

EXAMPLE_DIR="$PROJECT_DIR/solr-dump-importer/src/it/resources"

ISABELLE="$PROJECT_DIR/isabelle/bin/isabelle"
ISABELLE_EXTENSIONS_DIR="$SCIPT_DIR/../isabelle-extensions"

SCALA_OPTS="-J-Xmx2g -J-Xms2G"
DUMP_OPTS="-O $PROJECT_DIR/dump/example/ -d $EXAMPLE_DIR IAS-Example"

"$ISABELLE" scala $SCALA_OPTS "$ISABELLE_EXTENSIONS_DIR/dump_stable.scala" $DUMP_OPTS

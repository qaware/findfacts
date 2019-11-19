#!/bin/bash

# Base Directories
SCIPT_DIR="${0%/*}"
PROJECT_DIR="$SCIPT_DIR/.."
ISABELLE_DIR="$PROJECT_DIR/isabelle"
AFP_DIR="$PROJECT_DIR/afp"
ISABELLE_EXTENSIONS_DIR="$PROJECT_DIR/isabelle-extensions"

# Parameters
DUMP_DIR="$PROJECT_DIR/dump/all"
SCALA_OPTS="-J-Xmx8g -J-Xms4G"
EXCLUDES=("HOL-Proofs-ex")

# Get afp
hg clone https://bitbucket.org/isa-afp/afp-devel "$AFP_DIR"
# Get isabelle
hg clone https://isabelle.in.tum.de/repos/isabelle "$ISABELLE_DIR"

DUMP_OPTS="-A markup -O $DUMP_DIR ${EXCLUDES[*]/#/" -x "} -D "$AFP_DIR/thys" -a"

# Dump stuff
echo "Dumping isabelle + AFP theories..."
"$ISABELLE_DIR/bin/isabelle" scala $SCALA_OPTS "$ISABELLE_EXTENSIONS_DIR/dump_stable.scala" $DUMP_OPTS
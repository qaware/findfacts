#!/bin/bash

SCIPT_DIR="${0%/*}"
ISABELLE="$SCIPT_DIR/../isabelle/bin/isabelle"

"$ISABELLE" components -I
"$ISABELLE" components -a
"$ISABELLE" jedit -bf
"$ISABELLE" ocaml_setup
"$ISABELLE" ghc_setup
#!/bin/bash

git submodule init
git submodule update
isabelle/bin/isabelle components -I
isabelle/bin/isabelle components -a
isabelle/bin/isabelle jedit -bf
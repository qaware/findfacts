#!/bin/bash
${0%/*}/../isabelle/bin/isabelle dump -A markup,theory -O dump/example/ -d ${0%/*}/../solr-dump-importer/src/it/resources/ Example
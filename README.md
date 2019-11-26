# isabelle-afp-search
Project to make isabelle and the AFP easily searchable. Structured in:
- **common-solr**: common solr entities and access
- **common-utils**: common utilities
- **core**: search application core
- **solr-dump-importer**: importer pipeline to import isabelle `dump` into search index
- **webapp**: search ui layer as web application

## Building the project
Requirements: java 11 sdk, elm compiler for webapp
- **core**: `./sbt package` to build jar.
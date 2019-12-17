# isabelle-afp-search
Project to make isabelle and the AFP easily searchable. Structured in:
- **common**: common modules
- **search**: search application, with core module (**search-core**), web application (**search-webapp**), and frontend ui (**search-webapp-ui**).
- **importer**: importer pipeline to import isabelle `dump` into search index

## Usage
### Importer isabelle tool
Requirements: java 11, checked out and initialized **isabelle** submodule

```isabelle/bin/isabelle import_solr -?```

### Search webapp
Requirements

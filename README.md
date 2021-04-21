<p align="center">
  <img title="FindFacts" src="search-webapp/public/images/android-chrome-384x384.png" width="200px" />
</p>

[![CircleCI](https://circleci.com/gh/qaware/findfacts/tree/master.svg?style=svg)](https://circleci.com/gh/qaware/findfacts/tree/master)

# findfacts
Project to make Isabelle and the AFP easily searchable. Structured in:
- **common**: common modules
- **search**: search application, with core module (**search-core**), web application (**search-webapp**), and frontend ui (**search-webapp-ui**).
- **importer**: importer pipeline to import Isabelle `dump` into search index

## Usage
- Requirements: `java 15`
- Build: `./sbt -Dprofiles=ui,loader clean compile test it:test`
- Preparation: Initialize git submodules (`git submodule init && git submodule update`)

### Importer tool
Generally:
```shell
./sbt "project importer-isabelle" "run <OPTIONS>"
```
Use `-?` to get information about the tool usage.
Example invocation:
```shell
./sbt "project importer-isabelle" "run -r localhost:8983 -i 2021_Isabelle2021_AFP2021 -a dump/" 
``` 
[Isabelle settings](.isabelle/etc/settings): Increase memory a bit: `-Xss16m` `-Xmx8g`

### Search webapp
Run:
```shell
./sbt "project search-webapp" run
```

Build and publish docker image:
```shell
./sbt "project search-webapp" "docker:publish"
```

For deployment, see the [deployment repo](https://github.com/qaware/findfacts-deployment).

## Code style
This project uses the [databricks style guide](https://github.com/databricks/scala-style-guide) with some changes:

- __column width__: use 120.
- __implicits__: Only avoid them outside of well-known patterns, such as type-classes, implicit context, and pimp-my-library.
- __monadic chaining__: Use for-comprehensions to easily chain monads in an understandable and readable way.
- __multiple parameter lists__: Use multiple parameter list for partially applicable functions or to improve type inference.

Formatting is automated via scalafmt.

The `importer-isabelle` submodule instead adheres to the Isabelle code style.
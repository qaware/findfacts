# isabelle-afp-search
Project to make isabelle and the AFP easily searchable. Structured in:
- **common**: common modules
- **search**: search application, with core module (**search-core**), web application (**search-webapp**), and frontend ui (**search-webapp-ui**).
- **importer**: importer pipeline to import isabelle `dump` into search index

## Usage
Requirements: `java` 11

### Importer isabelle tool

Steps to run:
1. Check out and `cd` into repo
2. Check out and initialize **isabelle** submodule (or have a local isabelle installation)

   ```shell
   git submodule init
   git submodule update
   ```
   
3. Register `importer-isabelle` external component (for local installation, use your own `settings` file)

   ```shell
   echo 'init_component "path/to/isabelle-afp-search/importer-isabelle/target/scala-2.12"' >> isabelle/etc/settings
   ```
   
4. Run using sbt

    ```shell
    ./sbt "project importer-isabelle" "run <OPTIONS>"
   ```
    Use `-?` to get information about the tool usage. Example invocation:
    ```shell
   ./sbt "project importer-isabelle" "run ../dump localhost 8983" 
   ``` 

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
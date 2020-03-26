# isabelle-afp-search
Project to make Isabelle and the AFP easily searchable. Structured in:
- **common**: common modules
- **search**: search application, with core module (**search-core**), web application (**search-webapp**), and frontend ui (**search-webapp-ui**).
- **importer**: importer pipeline to import Isabelle `dump` into search index

## Usage
Requirements: `java` 11

### Importer Isabelle tool

#### From source
1. Check out and `cd` into repo
2. Check out, initialize and build **Isabelle** submodule
   ```shell
   git submodule init
   git submodule update
   ci-scripts/install-isabelle
   ```
   
3. Register `importer-isabelle` external component (for local installation, use your own `settings` file)
   ```shell
   echo 'init_component "path/to/isabelle-afp-search/importer-isabelle/target/scala-2.12"' >> isabelle/etc/settings
   ```
   
4. Run
    ```shell
    ./sbt "project importer-isabelle" "run <OPTIONS>"
   ```
    Use `-?` to get information about the tool usage. Example invocation:
    ```shell
   ./sbt "project importer-isabelle" "run ../dump localhost 8983" 
   ``` 

#### Using local Isabelle installation
1. Download published artifact (TODO publish)
2. Add Isabelle component (to Isabelle `etc/settings` file):
   ```shell
   init_component /path/to/download/folder
   ```
3. Run
   ```env
   isabelle dump_importer -?
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
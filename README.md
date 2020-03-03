# isabelle-afp-search
Project to make isabelle and the AFP easily searchable. Structured in:
- **common**: common modules
- **search**: search application, with core module (**search-core**), web application (**search-webapp**), and frontend ui (**search-webapp-ui**).
- **importer**: importer pipeline to import isabelle `dump` into search index

## Usage
### Importer isabelle tool
Requirements: `java` 11

Steps to run:
1. Check out and `cd` into repo
2. Check out and initialize **isabelle** submodule (or have a local isabelle installation)

   ```console
   git submodule init
   git submodule update
   ```
   
3. Register `importer-isabelle` external component (for local installation, use your own `settings` file)

   ```console
   echo 'init_component "path/to/isabelle-afp-search/importer-isabelle/target/scala-2.12"' >> isabelle/etc/settings
   ```
   
4. Run using sbt

    ```console
    ./sbt "project importer-isabelle" "run <OPTIONS>"
   ```
    Use `-?` to get information about the tool usage. Example invocation:
    ```console
   ./sbt "project importer-isabelle" "run ../dump localhost 8983" 
   ``` 

### Search webapp (deployment)
Requirements: `docker` >= 18, `docker-compose`

Steps to deploy:
1. Check out and `cd` into repo
2. Create and set application secret:

   ```console
   head -c 32 /dev/urandom | base64
   ```
   Set result as value in `deployment/app/app.env` for `APPLICATION_SECRET` key
3. Set hostname in `deployment/app/server.env`
4. Start infrastructure, then repeat with app and db:

   ```console
   cd deployment/infrastructure
   docker-compose up -d
   ```
   (If detach does not work for some reason, use `ctrl+z` to leave + run in background)

#### Services
You can then reach the following endpoints:
 - 80/443: reverse-proxy
 - 3000: app
 - 8983: solr
 - 514,601,6514: syslog
 
So make sure only ports 80 and 443 are exposed to the web.

Logs are collected in the `infrastructure_logs` volume (can also be accessed from `prod_syslog` container under `/var/log/syslog-daemon/`).

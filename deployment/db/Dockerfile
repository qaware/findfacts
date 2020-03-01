FROM solr:8.2.0

COPY --chown=solr:solr ./solr /var/solr

EXPOSE 8983

RUN "solr-foreground"
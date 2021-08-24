FROM teic/teidev-docker
RUN mkdir /opt/actions
COPY process.sh /opt/actions/process.sh
RUN chmod +x /opt/actions/process.sh
ENTRYPOINT ["/opt/actions/process.sh"]
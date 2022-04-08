FROM teic/teidev-docker

# Install TEI Stylesheets
RUN apt-get update && apt-get -y install apt-transport-https
RUN echo "deb https://packages.tei-c.org/deb/binary ./" >> /etc/apt/sources.list
RUN wget "https://packages.tei-c.org/deb/tei-c.pub.key.asc"
RUN apt-key add tei-c.pub.key.asc
RUN apt-get update && apt-get -y install tei-xsl

RUN mkdir /opt/actions
COPY process.sh /opt/actions/process.sh
RUN chmod +x /opt/actions/process.sh
ENTRYPOINT ["/opt/actions/process.sh"]
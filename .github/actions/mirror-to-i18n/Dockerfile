FROM debian:buster-slim
RUN apt-get update && apt-get -y install git
RUN mkdir /opt/actions
COPY process.sh /opt/actions/process.sh
ENTRYPOINT ["/opt/actions/process.sh"]
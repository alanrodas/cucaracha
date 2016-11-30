FROM ubuntu
MAINTAINER Alan Rodas Bonjour <alanrodas@gmail.com>

RUN apt-get update
RUN apt-get install -y \
    curl \
    gcc \
    git \
    make \
    nasm

VOLUME /opt
WORKDIR /opt

ENTRYPOINT '/bin/bash'

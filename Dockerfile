FROM fpco/stack-build-small:lts-19.10 AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    gcc \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /src
WORKDIR /src

# Dependencies
COPY stack.yaml package.yaml /src/
RUN stack install --dependencies-only

# App
COPY app /src/app
COPY src /src/src
COPY config /src/config
COPY restyle-path /src/restyle-path
RUN stack install

# Docker client
ENV DOCKER_ARCHIVE docker-17.03.1-ce.tgz
ENV DOCKER_SRC_URL https://get.docker.com/builds/Linux/x86_64/$DOCKER_ARCHIVE
RUN \
  curl -fsSLO "$DOCKER_SRC_URL" && \
  tar --strip-components=1 -xvzf "$DOCKER_ARCHIVE" -C /usr/local/bin

FROM ubuntu:18.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    gcc \
    git \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

# Build stage files
COPY --from=builder /root/.local/bin/restyler /bin/restyler
COPY --from=builder /root/.local/bin/restyle-path /bin/restyle-path
COPY --from=builder /usr/local/bin/docker /usr/local/bin/docker

ENV GIT_AUTHOR_NAME=Restyled.io
ENV GIT_AUTHOR_EMAIL=commits@restyled.io
ENV GIT_COMMITTER_NAME=Restyled.io
ENV GIT_COMMITTER_EMAIL=commits@restyled.io

VOLUME /code
WORKDIR /code

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["--help"]

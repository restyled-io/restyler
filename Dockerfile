# Build stage
FROM quay.io/haskell_works/stack-build-minimal:18.04 as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

RUN stack upgrade

RUN mkdir -p /src
WORKDIR /src

COPY stack.yaml /src/
RUN stack setup

COPY package.yaml /src/
RUN stack install --dependencies-only

COPY app /src/app
COPY src /src/src
COPY LICENSE /src/
RUN stack install

# Runtime
FROM ubuntu:18.04

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get -y install \
    curl \
    git \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

COPY --from=builder /root/.local/bin/restyler /bin/restyler

# Install docker
ENV DOCKER_ARCHIVE docker-17.03.1-ce.tgz
ENV DOCKER_SRC_URL https://get.docker.com/builds/Linux/x86_64/$DOCKER_ARCHIVE
RUN \
  curl -fsSLO "$DOCKER_SRC_URL" && \
  tar --strip-components=1 -xvzf "$DOCKER_ARCHIVE" -C /usr/local/bin

ENV GIT_AUTHOR_NAME=Restyled.io
ENV GIT_AUTHOR_EMAIL=commits@restyled.io
ENV GIT_COMMITTER_NAME=Restyled.io
ENV GIT_COMMITTER_EMAIL=commits@restyled.io

ENTRYPOINT ["/bin/restyler"]
CMD ["--help"]

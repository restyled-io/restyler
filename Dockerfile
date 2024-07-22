FROM ubuntu:24.04
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

# Act
RUN \
  curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/nektos/act/master/install.sh | bash

# GH
RUN \
  cd /tmp && \
  curl -sSf https://github.com/cli/cli/releases/download/v2.53.0/gh_2.53.0_linux_amd64.tar.gz | tar xvf - && \
  cp -v gh_*/bin/gh /usr/local && \
  rm -rf gh_*

RUN mkdir -p /tmp/restyled/.github/workflows
WORKDIR /tmp/restyled
ENV HOST_DIRECTORY=/tmp/restyled

COPY agent-workflow.yml /tmp/restyled/.github/workflows/default.yml
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["--help"]

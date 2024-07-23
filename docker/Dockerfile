FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    curl && \
  rm -rf /var/lib/apt/lists/*

# Act
RUN \
  curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/nektos/act/master/install.sh | bash

# GH
RUN \
  cd /tmp && \
  curl -L -sSf https://github.com/cli/cli/releases/download/v2.53.0/gh_2.53.0_linux_amd64.tar.gz | tar xzvf - && \
  cp -v gh_*/bin/gh /usr/local/bin && \
  rm -rf gh_*

RUN mkdir -p /opt/workflows
COPY agent.yml /opt/workflows/
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["--help"]

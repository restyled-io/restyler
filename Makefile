LOCAL_IMAGE   ?= restyled/restyler
RELEASE_IMAGE ?= $(LOCAL_IMAGE)

DOCKER_USERNAME ?= x
DOCKER_PASSWORD ?= x

# Used by local integration test
RESTYLER_GITHUB_APP_KEY ?= \
  $(HOME)/downloads/restyled/restyled-io-development.2017-09-19.private-key.pem

all: setup build lint test

release: clean build lint test test.core image.build image.release

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: clean
clean:
	stack clean

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: lint
lint:
	hlint .
	weeder .

.PHONY: test
test:
	stack test

.PHONY: test.core
test.core:
	cram test/core

# Restyles restyled-io/demo#1
.PHONY: test.integration
test.integration: image.build
	docker run --rm \
	  --env DEBUG=1 \
	  --volume /tmp:/tmp \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  "$(LOCAL_IMAGE)" \
	    --github-app-id 5355 \
	    --github-app-key "$$(< "$(RESTYLER_GITHUB_APP_KEY)")" \
	    --installation-id 58920 \
	    --owner restyled-io \
	    --repo demo \
	    --pull-request 1

.PHONY: install
install:
	stack install

.PHONY: image.build
image.build:
	docker build --tag "$(LOCAL_IMAGE)" .

.PHONY: image.release
image.release:
	@docker login \
	  --username "$(DOCKER_USERNAME)" \
	  --password "$(DOCKER_PASSWORD)" || \
	  echo "docker login failed, release may fail."
	docker tag "$(LOCAL_IMAGE)" "$(RELEASE_IMAGE)"
	docker push "$(RELEASE_IMAGE)"

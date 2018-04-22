LOCAL_IMAGE   ?= restyled/restyler
RELEASE_IMAGE ?= $(LOCAL_IMAGE)

DOCKER_USERNAME ?= x
DOCKER_PASSWORD ?= x

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
	@# Use an explicit list to avoid test/core
	hlint app core src test/SpecHelper.hs test/Restyler
	weeder .

.PHONY: test
test:
	stack test

.PHONY: test.core
test.core:
	cram test/core

.PHONY: test.integration
test.integration: image.build
	docker run --rm \
	  --env DEBUG=1 \
	  --env GITHUB_ACCESS_TOKEN \
	  --volume /tmp:/tmp \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  "$(LOCAL_IMAGE)" "restyled-io/demo#1"

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

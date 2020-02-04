# This PR has differences such that all Restylers known at the time I made it
# will run, making it a great test PR.
INTEGRATION_PULL_REQUEST ?= restyled-io/demo\#46
INTEGRATION_RESTYLER_IMAGE ?= restyled/restyler
INTEGRATION_RESTYLER_TAG ?= :latest
INTEGRATION_RESTYLER_BUILD ?= 1

all: setup setup.lint setup.tools build lint test

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	# Avoid ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)
	stack build $(STACK_ARGUMENTS) -j 1 Cabal
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  hlint \
	  weeder

.PHONY: setup.tools
setup.tools:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  brittany \
	  fast-tags \
	  stylish-haskell

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) --pedantic --test --no-run-tests

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint app src test
	# Weeder doesn't work with stack-2.0 :(
	# stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) --test

.PHONY: watch
watch:
	stack build $(STACK_ARGUMENTS) --fast --pedantic --test --file-watch

.PHONY: image
image:
	docker run -it --rm \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  --volume "$(HOME)"/.docker/config.json:/root/.docker/config.json:ro \
	  --volume "$(PWD)":/build:ro \
	  --workdir /build \
	  restyled/ops:v5 docker-build-remote-cache \
	  restyled/restyler \
	  --build-arg "REVISION=testing"

.PHONY: test.integration
test.integration:
	if [ "$(INTEGRATION_RESTYLER_BUILD)" -eq 1 ]; then \
	  $(MAKE) image && \
	  docker tag restyled/restyler \
	    $(INTEGRATION_RESTYLER_IMAGE)$(INTEGRATION_RESTYLER_TAG); \
	fi
	docker run -it --rm \
	  --env DEBUG=1 \
	  --env GITHUB_ACCESS_TOKEN=$$(./bin/get-access-token) \
	  --volume /tmp:/tmp \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  $(INTEGRATION_RESTYLER_IMAGE)$(INTEGRATION_RESTYLER_TAG) \
	    --job-url https://example.com \
	    --color=always "$(INTEGRATION_PULL_REQUEST)"

RESTYLERS_VERSION ?=

.PHONY: restylers_version
restylers_version:
	[ -n "$(RESTYLERS_VERSION)" ]
	git stash --include-untracked || true
	git checkout master
	git pull --rebase
	sed -i 's/^\(restylers_version: "\).*"$$/\1$(RESTYLERS_VERSION)"/' config/default.yaml
	git commit config/default.yaml -m "Bump default restylers_version"
	git push

DOC_ROOT = $(shell stack path --work-dir .stack-work-docs --local-doc-root)
DOC_S3_PREFIX = /restyler

.PHONY: docs
docs:
	stack $(STACK_ARGUMENTS) --work-dir .stack-work-docs build --haddock
	find .stack-work-docs -type f -name '*.html' -exec \
	  sed -i 's|$(DOC_ROOT)|$(DOC_S3_PREFIX)|g' {} +
	aws s3 sync --acl public-read --delete $(DOC_ROOT)/ \
	  s3://docs.restyled.io$(DOC_S3_PREFIX)/

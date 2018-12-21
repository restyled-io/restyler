# This PR has differences such that all Restylers known at the time I made it
# will run, making it a great test PR.
INTEGRATION_PULL_REQUEST ?= restyled-io/restylers\#3

all: setup build lint test

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install --copy-compiler-tool \
	  brittany \
	  fast-tags \
	  hlint \
	  stylish-haskell \
	  weeder

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: lint
lint:
	stack exec hlint app src test
	stack exec weeder .

.PHONY: test
test:
	stack test

.PHONY: docs
docs:
	stack --work-dir .stack-work-docs build --haddock
	aws s3 sync --acl public-read --delete \
	  $$(stack path --work-dir .stack-work-docs --local-doc-root)/ \
	  s3://docs.restyled.io/restyler/

.PHONY: test.integration
test.integration:
	docker build --tag restyled/restyler .
	docker run --rm \
	  --env DEBUG=1 \
	  --env GITHUB_ACCESS_TOKEN \
	  --volume /tmp:/tmp \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  restyled/restyler --color=always "$(INTEGRATION_PULL_REQUEST)"

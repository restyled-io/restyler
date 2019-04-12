# This PR has differences such that all Restylers known at the time I made it
# will run, making it a great test PR.
INTEGRATION_PULL_REQUEST ?= restyled-io/restylers\#6
INTEGRATION_RESTYLER_IMAGE ?= restyled/restyler
INTEGRATION_RESTYLER_TAG ?= :latest

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
	stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) --test

.PHONY: test.integration
test.integration:
	docker build --tag restyled/restyler .
	docker run --rm \
	  --env DEBUG=1 \
	  --env GITHUB_ACCESS_TOKEN \
	  --volume /tmp:/tmp \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  $(INTEGRATION_RESTYLER_IMAGE)$(INTEGRATION_RESTYLER_TAG) \
	    --color=always "$(INTEGRATION_PULL_REQUEST)"

.PHONY: docs
docs:
	stack $(STACK_ARGUMENTS) --work-dir .stack-work-docs build --haddock
	aws s3 sync --acl public-read --delete \
	  $$(stack path --work-dir .stack-work-docs --local-doc-root)/ \
	  s3://docs.restyled.io/restyler/

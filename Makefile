# This PR has differences such that all Restylers known at the time I made it
# will run, making it a great test PR.
INTEGRATION_PULL_REQUEST ?= restyled-io/restylers\#3

all: setup build lint test

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  brittany \
	  fast-tags \
	  hlint \
	  stylish-haskell \
	  weeder

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

.PHONY: docs
docs:
	stack $(STACK_ARGUMENTS) --work-dir .stack-work-docs build --haddock
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

# To release is just to tag. Quay.io will pick it up and build the image.
.PHONY: release
release:
	[ -n "$(VERSION)" ]
	git tag -a "v$(VERSION)" "v$(VERSION)"
	git push --tags

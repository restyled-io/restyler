all: setup build lint test

release: clean build lint test image.build image.release

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

.PHONY: test.ci
test.ci:
	# skip callRestylers because it requires docker
	stack test --test-arguments "--skip callRestylers"

.PHONY: install
install:
	stack install

.PHONY: image.build
image.build:
	docker build --tag "restyled/restyler$(IMAGE_TAG)" .

.PHONY: image.release
image.release:
	docker push "restyled/restyler$(IMAGE_TAG)"

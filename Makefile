all: setup setup.lint setup.tools build lint test

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests

.PHONY: setup.lint
setup.lint:
	stack install --copy-compiler-tool hlint weeder

.PHONY: setup.tools
setup.tools:
	stack install --copy-compiler-tool \
	  brittany \
	  fast-tags \
	  stylish-haskell

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: lint
lint:
	stack exec hlint app src test
	stack exec weeder .

.PHONY: test
test:
	stack build --test

.PHONY: test.integration
test.integration: image
	AWS_PROFILE=restyled-ci \
	  restyled promote --image restyled/restyler:edge --debug stable

.PHONY: watch
watch:
	stack build --fast --pedantic --test --file-watch

.PHONY: image
image:
	docker build \
	  --tag restyled/restyler:edge \
	  .

AWS ?= aws --profile restyled-ci

DOC_BUCKET = $(shell \
  $(AWS) cloudformation describe-stacks \
    --stack-name sites-docs \
    --query 'Stacks[*].Outputs[?OutputKey==`BucketName`].OutputValue' \
    --output text \
)

DOC_DISTRIBUTION_ID = $(shell \
  $(AWS) cloudformation describe-stacks \
    --stack-name sites-docs \
    --query 'Stacks[*].Outputs[?OutputKey==`DistributionId`].OutputValue' \
    --output text \
)

DOC_ROOT = $(shell stack path --local-doc-root)
DOC_S3_PREFIX = /restyler

.PHONY: docs
docs:
	[ -n "$$STACK_WORK_DIR" ]
	stack build --haddock
	find "$$STACK_WORK_DIR" -type f -name '*.html' -exec \
	  sed -i 's|$(DOC_ROOT)|$(DOC_S3_PREFIX)|g' {} +
	$(AWS) s3 sync --acl public-read --delete $(DOC_ROOT)/ \
	  s3://$(DOC_BUCKET)$(DOC_S3_PREFIX)/
	$(AWS) cloudfront create-invalidation \
	  --distribution-id $(DOC_DISTRIBUTION_ID) --paths "$(DOC_S3_PREFIX)/*"

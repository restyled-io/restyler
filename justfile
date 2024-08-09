# Setup, lint, build and test
all: setup build test lint

# Install dependencies
setup:
  stack setup
  stack build --dependencies-only --test --no-run-tests
  stack install --copy-compiler-tool \
    apply-refact \
    fast-tags \
    fourmolu \
    hlint \
    weeder

# Build but do not run tests
build:
  stack build --fast --pedantic --test --no-run-tests

# Build and test
test:
  stack build --fast --pedantic --test

# Lint and format the project
lint:
  stack exec -- fourmolu -i app src test
  stack exec -- hlint app src test
  stack exec -- weeder --require-hs-files
  stack lint-extra-deps

# Rebuild and test on file-changes
watch:
  stack build --fast --pedantic --test --file-watch

headroom:
  stack exec -- headroom run -r
  stack exec -- fourmolu -i app src test

# aws := "aws --profile restyled-ci"

# doc_bucket := `
#   {{aws}} cloudformation describe-stacks \
#     --stack-name sites-docs \
#     --query 'Stacks[*].Outputs[?OutputKey==`BucketName`].OutputValue' \
#     --output text \
# `

# doc_distribution_id := `
#   {{aws}} cloudformation describe-stacks \
#     --stack-name sites-docs \
#     --query 'Stacks[*].Outputs[?OutputKey==`DistributionId`].OutputValue' \
#     --output text \
# `

# doc_root := `stack path --local-doc-root`
# doc_s3_prefix := /restyler

# docs:
#   [ -n "$$STACK_WORK_DIR" ]
#   stack build --haddock
#   find "$$STACK_WORK_DIR" -type f -name '*.html' -exec \
#     sed -i 's|{{doc_root}}|{{doc_s3_prefix}}|g' {} +
#   {{aws}} s3 sync --acl public-read --delete {{doc_root}}/ \
#     s3://{{doc_bucket}}{{doc_s3_prefix}}/
#   {{aws}} cloudfront create-invalidation \
#     --distribution-id {{doc_distribution_id}} --paths "{{doc_s3_prefix}}/*"

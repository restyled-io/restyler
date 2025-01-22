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
  stack exec -- headroom run
  stack exec -- fourmolu -i app src test

install-root := `stack path --local-install-root`
dist         := 'restyler-' + `uname -s | tr '[:upper:]' '[:lower:]'` + '-' + `uname -m`
ext          := 'tar.gz'

dist:
  mkdir -p '{{dist}}'
  cp -v '{{install-root}}'/bin/* '{{dist}}'
  tar czf '{{dist}}.{{ext}}' '{{dist}}'

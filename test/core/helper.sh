run_restyler() {
  printf -- "- %s\n" "$1" > .restyled.yaml; shift
  "$CRAMTMP/restyler-core" "$@"
  git diff "$@"
}

set -e

(
  cd "$TESTDIR"/../..
  stack install --local-bin-path="$CRAMTMP"
) >/dev/null 2>&1
echo "If you don't see this, stack install failed"

mkdir repo
cd repo
cp "$TESTDIR"/fixtures/* .

{
  git init
  git add .
  git commit -m "Add fixture files"
} >/dev/null

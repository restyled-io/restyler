#!/usr/bin/env bash
set -euo pipefail

repo=
number=

while [[ -n "$1" ]]; do
  if [[ "$1" =~ ([^/]+)/([^/]+)#([0-9]+) ]]; then
    repo=${BASH_REMATCH[1]}/${BASH_REMATCH[2]}
    number=${BASH_REMATCH[3]}
    break
  fi

  shift
done

if [[ -z "$repo" ]] || [[ -z "$number" ]]; then
  echo "Unable to parse OWNER/REPO#NUMBER argument: $*" >&2
  exit 64
fi

cwd=$(mktemp -d "restyler-XXXXXX")
event=$(mktemp)

export GH_TOKEN=$GITHUB_ACCESS_TOKEN
export HOST_DIRECTORY="$cwd"

{
  echo '{"pull_request":'
  gh api "repos/$repo/pulls/$number"
  echo '}'
} >"$event"

cd "$cwd"

act \
  --bind \
  --env GITHUB_REPOSITORY="$repo" \
  --env HOST_DIRECTORY \
  --eventpath "$event" \
  --platform ubuntu-latest=catthehacker/ubuntu:act-latest \
  --secret GITHUB_TOKEN="$GITHUB_ACCESS_TOKEN" \
  --workflows /opt/workflows

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

  if [[ "$1" == '--job-url' ]]; then
    shift
    job_url=$1
  fi

  shift
done

if [[ -z "$repo" ]] || [[ -z "$number" ]]; then
  echo "Unable to parse OWNER/REPO#NUMBER argument: $*" >&2
  exit 64
fi

event=$(mktemp)
{
  echo '{"pull_request":'
  gh api "repos/$repo/pulls/$number"
  echo '}'
} >"$event"

exec act \
  --env HOST_DIRECTORY="$PWD" \
  --env JOB_URL="$job_url" \
  --env GITHUB_REPOSITORY="$repo" \
  --secret GITHUB_TOKEN="$GITHUB_ACCESS_TOKEN" \
  --eventpath "$event" \
  --bind

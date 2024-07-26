#!/bin/sh
usage() {
  cat <<'EOM'
install [-t TAG] [-p PREFIX]

Options:
  -t TAG        Choose a specific tag to install. If omitted, latest is used.
  -p PREFIX     Install to PREFIX/bin. Default is /usr/local.

  -h            Show this help

The script must be run as a user with write permission in PREFIX. For the
default PREFIX, this means root.

Setting options in a curl|sh invocation is done with `-s --':

  curl ... | sh -s -- -p ~/.local

EOM
}

tag=
prefix=/usr/local

while getopts t:p:h opt; do
  case "$opt" in
    t)
      tag=$OPTARG
      ;;
    p)
      prefix=$OPTARG
      ;;
    h)
      usage
      exit 0
      ;;
    \?)
      usage >&2
      exit 64
      ;;
  esac
done

shift $((OPTIND - 1))

if [ -z "$tag" ]; then
  tag=$(curl -sSf https://api.github.com/repos/restyled-io/restyler/releases |
    grep -o '"tag_name": ".*"' |
    sed 's/^.*: "//; s/"$//' |
    grep -v -- '-rc$' |
    head -n 1)
fi

if [ -z "$tag" ]; then
  echo "Unable to determine latest tag" >&2
  exit 1
fi

cd /tmp || exit 1

artifact_name=restyler-$(uname -s | tr '[:upper:]' '[:lower:]')-$(uname -m)
artifact_url=https://github.com/restyled-io/restyler/releases/download/$tag/$artifact_name.tar.gz

echo "Downloading $tag/$artifact_name..."

if ! curl -sSf -L "$artifact_url" | tar xzf -; then
  cat >&2 <<EOM
Download failed

1. Does https://github.com/restyled-io/restyler/releases/tag/$tag exist?
2. Does it have an artifact named $artifact_name?
3. Does $artifact_url exist?

To request an artifact be added for your platform, or to report a bug with this
script, go to https://github.com/restyled-io/restyler/issues.
EOM
  exit 1
fi

echo "Installing binaries in $prefix/bin"
for bin in "$artifact_name"/*; do
  if [ -x "$bin" ]; then
    cp -v "$bin" "$prefix"/bin
  fi
done

echo "Cleaning up..."
rm -rf "$artifact_name"

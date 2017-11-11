# Restyler

The restyling process, as a CLI.

## Installation

```console
make
make install
```

Requires [stack](https://docs.haskellstack.org/en/stable/README/). Creates
`~/.local/bin/restyler`, which must be on `$PATH`.

## Usage

```
Usage: restyler --github-app-id ID --github-app-key KEY --installation-id ID
                --owner NAME --repo NAME --pull-request NUMBER
                [--restyled-root URL]
  Restyle a GitHub Pull Request

Available options:
  --github-app-id ID       GitHub App Id
  --github-app-key KEY     GitHub App Key
  --installation-id ID     Installation Id
  --owner NAME             Owner
  --repo NAME              Repo
  --pull-request NUMBER    Pull Request
  --restyled-root URL      Root for restyled.io
  -h,--help                Show this help text
```

## Docker

To pull and run the latest version from Docker Hub:

```console
docker run --rm \
  --volume /tmp:tmp \
  --volume /var/run/docker.sock:/var/run/docker.sock \
  restyled/restyler --help
```

To build the image locally:

```console
make image.build
```

To release the built image to Docker Hub:

```console
make image.release
```

Assumes you've already `docker login`-ed to the registry.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)

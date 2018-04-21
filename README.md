# Restyler

The restyling process, as a CLI.

## Usage

To restyle a given Pull Request, you would need:

1. The GitHub App Id
1. The contents of that App's PEM private key
1. The Installation Id for the Repository's installed instance of that App
1. The Repository Owner and Name (e.g. `restyled.io/demo`)
1. The Pull Request Number to restyle

```console
docker run --rm \
  --volume /tmp:tmp \
  --volume /var/run/docker.sock:/var/run/docker.sock \
  restyled/restyler \
    --github-app-id ... \
    --github-app-key ... \
    --installation-id ... \
    --owner ... \
    --repo ... \
    --pull-request ...
```

## Development

```console
make
```

Requires [stack](https://docs.haskellstack.org/en/stable/README/).

## Tests

```console
make test
```

### Core

Exercises every Restyler against local fixtures:

```console
make test.core
```

### Integration

End-to-end test that restyles a public Pull Request in our `demo` Repository:

```console
make test.integration
```

## Release

```console
make release
```

Assumes you've already `docker login`-ed to the registry.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)

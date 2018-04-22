# Restyler

The restyling process, as a CLI.

## Usage

```console
docker run --rm \
  --env "GITHUB_ACCESS_TOKEN=<access-token>" \
  --volume /tmp:tmp \
  --volume /var/run/docker.sock:/var/run/docker.sock \
  restyled/restyler "<owner>/<name>#<number>"
```

**NOTE**: The Access Token you use will determine some of the resulting
behavior. In production, we use a token provisioned for an installed instance of
our GitHub App, which ensures the restyled PRs and comments appear as authored
by our App. If you use a Personal Access Token, the restyled PRs and comments
will be authored by your user.

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
make test.integration GITHUB_ACCESS_TOKEN=$(bin/get-access-token)
```

## Release

```console
make release
```

Assumes you've already `docker login`-ed to the registry.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)

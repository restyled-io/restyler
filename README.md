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

### Integration

End-to-end test that restyles an example Pull Request:

```console
make test.integration
```

## `restyle-path`

See [bin/restyle-path](./bin/restyle-path).

## LICENSE

Restyled is source-available, [Commons Claused][cc] licensed. For a detailed
description of another project's rationale for this licensing model, one with
which I agree, see [here][level].

[cc]: https://commonsclause.com/
[level]: https://leveljournal.com/source-available-licensing

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE) | [CONTRIBUTING][]

[contributing]: https://github.com/restyled-io/restyled.io/wiki/Contributing-to-Restyled

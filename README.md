# Restyler

The restyling process, as a CLI.

## Usage

### GitHub Actions

```yaml
- uses: actions/checkout@v4
  with:
    ref: ${{ github.event.pull_request.head.ref }}
- uses: restyled-io/actions/setup@v1
- uses: restyled-io/actions/run@v1
```

### Locally

```console
curl --proto '=https' --tlsv1.2 -sSf \
  https://raw.githubusercontent.com/restyled-io/restyler/main/install | sudo sh
```

```console
restyle --help
```

## Development

```console
just
```

Requires [stack](https://docs.haskellstack.org/en/stable/README/).

## Tests

```console
just test
```

### Integration

End-to-end test that restyles an example Pull Request:

```console
just test-integration
```

## LICENSE

Restyled is source-available, [Commons Claused][cc] licensed. For a detailed
description of another project's rationale for this licensing model, one with
which I agree, see [here][level].

[cc]: https://commonsclause.com/
[level]: https://web.archive.org/web/20181120030157/https://leveljournal.com/source-available-licensing

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE) | [CONTRIBUTING][]

[contributing]: https://github.com/restyled-io/restyled.io/wiki/Contributing-to-Restyled

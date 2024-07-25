# Restyler

The restyling process, as a CLI.

## Installation

```console
curl --proto '=https' --tlsv1.2 -sSf \
  https://raw.githubusercontent.com/restyled-io/restyler/main/install | sudo sh
```
## Usage

```console
Usage: restyle [--debug] [--trace] [--color WHEN] [--host-directory DIRECTORY]
               [--no-commit] [--fail-on-differences] PATH [PATH]

  Restyle local files

Available options:
  --debug                  Enable debug logging
  --trace                  Enable trace logging
  --color WHEN             When to use color: always|never|auto
  --host-directory DIRECTORY
                           Working directory on host, if dockerized
  --no-commit              Don't make commits for restyle changes
  --fail-on-differences    Exit non-zero if differences were found
  -h,--help                Show this help text

Available environment variables:
  FAIL_ON_DIFFERENCES    Exit non-zero if differences were
                         found
  HOST_DIRECTORY         Working directory on host, if
                         dockerized
  NO_COMMIT              Don't make commits for restyle
                         changes
  RESTYLER_CPU_SHARES    Run restylers with
                         --cpu-shares=<number>
  RESTYLER_MEMORY        Run restylers with
                         --memory=<number>[b|k|m|g]
  RESTYLER_NO_NET_NONE   Run restylers without --net=none
  UNRESTRICTED           Run restylers without CPU or
                         Memory restrictions
```

### GitHub Actions

See https://github.com/restyled-io/actions#readme.

## Development

```console
just
```

Requires [stack](https://docs.haskellstack.org/en/stable/README/).

## Tests

```console
just test
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

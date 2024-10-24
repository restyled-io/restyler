# Restyler

The restyling process, as a CLI.

## Installation

```console
curl --proto '=https' --tlsv1.2 -sSf \
  https://raw.githubusercontent.com/restyled-io/restyler/main/install | sudo sh
```

## Usage

See [`restyle(1)`][restyle.1] and [`restyled.yaml(5)`][restyled.yaml.5].

[restyle.1]: https://restyled-io.github.io/restyler/man-pages/restyle.1
[restyled.yaml.5]: https://restyled-io.github.io/restyler/man-pages/restyled.yaml.5

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

This project is licensed AGPLv3. See [COPYING](./COPYING).

---

[CHANGELOG](./CHANGELOG.md)

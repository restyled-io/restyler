# Restyler

The restyling process, as a CLI.

## Installation

```console
curl --proto '=https' --tlsv1.2 -sSf \
  https://raw.githubusercontent.com/restyled-io/restyler/main/install | sudo sh
```

## Usage

```console
Usage: restyle [--debug] [--trace] [--color WHEN] [--dry-run]
               [--fail-on-differences] [--host-directory DIRECTORY]
               [--image-cleanup] [--manifest FILE] [--no-commit] [--no-clean]
               [--no-pull] PATH [PATH]

  Restyle local files

Available options:
  --debug                  Enable debug logging
  --trace                  Enable trace logging
  --color WHEN             When to use color: always|never|auto
  --dry-run                Don't docker-pull or docker-run Restylers
  --fail-on-differences    Exit non-zero if differences were found
  --host-directory DIRECTORY
                           Working directory on host, if dockerized
  --image-cleanup          Remove pulled restyler images after restyling
  --manifest FILE          Restylers manifest to use
  --no-commit              Don't make commits for restyle changes
  --no-clean               Don't run git-clean after restyling
  --no-pull                Don't docker-pull images before docker-run
  -h,--help                Show this help text

Available environment variables:
  DRY_RUN                Don't docker-pull or docker-run
                         Restylers
  FAIL_ON_DIFFERENCES
                         Exit non-zero if differences were
                         found
  HOST_DIRECTORY         Working directory on host, if
                         dockerized
  IMAGE_CLEANUP          Remove pulled restyler images
                         after restyling
  LOG_BREAKPOINT
  LOG_COLOR
  LOG_CONCURRENCY
  LOG_DESTINATION
  LOG_FORMAT
  LOG_LEVEL
  MANIFEST               Restylers manifest to use
  NO_CLEAN               Don't run git-clean after
                         restyling
  NO_COLOR
  NO_COMMIT              Don't make commits for restyle
                         changes
  NO_PULL                Don't docker-pull images before
                         docker-run
  RESTYLER_CPU_SHARES
                         Run restylers with
                         --cpu-shares=<number>
  RESTYLER_MEMORY        Run restylers with
                         --memory=<number>[b|k|m|g]
  RESTYLER_NO_NET_NONE
                         Run restylers without --net=none
  TERM
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

This project is licensed AGPLv3. See [COPYING](./COPYING).

---

[CHANGELOG](./CHANGELOG.md)

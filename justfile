install-root := `stack path --local-install-root`
version      := `stack exec -- restyle --version | cut -d' ' -f 2`
dist         := 'restyler-' + `uname -s | tr '[:upper:]' '[:lower:]'` + '-' + `uname -m`
ext          := 'tar.gz'

headroom:
  headroom run
  fourmolu -i app src test

dist:
  mkdir -p '{{dist}}'/{bin,doc,completion}

  cp '{{install-root}}'/bin/restyle '{{dist}}'/bin/restyle

  '{{install-root}}'/bin/restyle __render-docs-man1__ > '{{dist}}'/doc/restyle.1.ronn
  '{{install-root}}'/bin/restyle __render-docs-man5__ > '{{dist}}'/doc/restyled.yaml.5.ronn
  ronn --manual '{{version}}' --organization "Restyled" --roff '{{dist}}'/doc/*.ronn

  cp Makefile '{{dist}}'/Makefile

  tar czf '{{dist}}.{{ext}}' '{{dist}}'

dist-clean:
  rm -f '{{dist}}.{{ext}}'
  rm -f -r '{{dist}}'

dist-install:
  tar xzf '{{dist}}.{{ext}}' -C /tmp
  cd '/tmp/{{dist}}' && make install PREFIX=$HOME/.local

dist-check:
  stack clean
  stack build --pedantic
  just dist dist-install dist-clean

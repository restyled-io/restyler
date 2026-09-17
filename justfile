install-root := `stack path --local-install-root`
dist         := 'restyler-' + `uname -s | tr '[:upper:]' '[:lower:]'` + '-' + `uname -m`
ext          := 'tar.gz'

docs-clean:
  git clean -fdx doc

docs-nroff:
  restyle __render-docs-man1__ > doc/restyle.1
  restyle __render-docs-man5__ > doc/restyled.yaml.5

docs-html:
  cd doc && mandoc -T html \
      -O 'man=./%N.%S.html;https://manpages.org/%N.%S,style=./style.css' \
      < restyle.1 \
      > restyle.1.html
  cd doc && mandoc -T html \
      -O 'man=./%N.%S.html;https://manpages.org/%N.%S,style=./style.css' \
      < restyled.yaml.5 \
      > restyled.yaml.5.html

headroom:
  headroom run
  fourmolu -i app src test

dist:
  mkdir -p '{{dist}}'
  cp -v '{{install-root}}'/bin/* '{{dist}}'
  tar czf '{{dist}}.{{ext}}' '{{dist}}'

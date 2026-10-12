install-root := `stack path --local-install-root`
dist         := 'restyler-' + `uname -s | tr '[:upper:]' '[:lower:]'` + '-' + `uname -m`
ext          := 'tar.gz'

headroom:
  headroom run
  fourmolu -i app src test

dist:
  mkdir -p '{{dist}}'
  cp -v '{{install-root}}'/bin/* '{{dist}}'
  tar czf '{{dist}}.{{ext}}' '{{dist}}'

mandoc-options := 'man=./%N.%S.html;https://manpages.org/%N.%S,style=./style.css'

docs:
  stack exec restyle __render-docs-man1__ > doc/restyle.1
  stack exec restyle __render-docs-man5__ > doc/restyled.yaml.5

[working-directory: 'doc']
docs-html:
  mandoc -T html -O '{{mandoc-options}}' < restyle.1 > restyle.1.html
  mandoc -T html -O '{{mandoc-options}}' < restyled.yaml.5 > restyled.yaml.5.html

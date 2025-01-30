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

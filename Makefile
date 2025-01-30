PREFIX ?= /usr/local

.PHONY: install
install:
	mkdir -p \
	  "$(DESTDIR)$(PREFIX)"/bin \
	  "$(DESTDIR)$(PREFIX)"/share/man/man1 \
	  "$(DESTDIR)$(PREFIX)"/share/man/man5 \
	  "$(DESTDIR)$(PREFIX)"/share/bash-completion/completions \
	  "$(DESTDIR)$(PREFIX)"/share/zsh/site-functions \
	  "$(DESTDIR)$(PREFIX)"/share/fish/vendor_completions.d
	cp -v bin/restyle         "$(DESTDIR)$(PREFIX)"/bin/restyle
	cp -v doc/restyle.1       "$(DESTDIR)$(PREFIX)"/share/man/man1/restyle.1
	cp -v doc/restyled.yaml.5 "$(DESTDIR)$(PREFIX)"/share/man/man5/restyled.yaml.5
	"$(DESTDIR)$(PREFIX)"/bin/restyle --bash-completion-script "$(PREFIX)"/bin/restyle > "$(DESTDIR)$(PREFIX)"/share/bash-completion/completions/restyle_
	"$(DESTDIR)$(PREFIX)"/bin/restyle --zsh-completion-script  "$(PREFIX)"/bin/restyle > "$(DESTDIR)$(PREFIX)"/share/zsh/site-functions/_restyle
	"$(DESTDIR)$(PREFIX)"/bin/restyle --fish-completion-script "$(PREFIX)"/bin/restyle > "$(DESTDIR)$(PREFIX)"/share/fish/vendor_completions.d/restyle.fish

.PHONY: uninstall
uninstall:
	$(RM) "$(DESTDIR)$(PREFIX)"/bin/restyle
	$(RM) "$(DESTDIR)$(PREFIX)"/share/man/man1/restyle.1
	$(RM) "$(DESTDIR)$(PREFIX)"/share/man/man5/restyled.yaml.5
	$(RM) "$(DESTDIR)$(PREFIX)"/share/bash-completion/completions/restyle_
	$(RM) "$(DESTDIR)$(PREFIX)"/share/zsh/site-functions/_restyle
	$(RM) "$(DESTDIR)$(PREFIX)"/share/fish/vendor_completions.d/restyle.fish

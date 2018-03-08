.PHONY: all
all: ;

.PHONY: update
update:
  git pull origin master
	git submodule init
  git submodule update

.PHONY: install
install: update
	@./install.bash

.PHONY: clean
clean:
  rcdn

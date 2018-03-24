.PHONY: all
all: ;

.PHONY: update
update:
	@git fetch origin
	@git pull origin master
	@git submodule update --init --remote

.PHONY: install
install: update
	@./install.bash

.PHONY: clean
clean:
	rcdn

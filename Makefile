.PHONY: all
all: ;

.PHONY: update
update:
	@git submodule update --init --remote

.PHONY: install
install: update
	@./install.bash

.PHONY: clean
clean:
	rcdn

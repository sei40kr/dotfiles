.PHONY: all
all: ;

.PHONY: update
update:
	@./update.bash

.PHONY: install
install: update
	@./install.bash

.PHONY: clean
clean:
	rcdn

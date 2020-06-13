FLAGS := -I "config=$$(pwd)/config" \
		 -I "packages=$$(pwd)/packages"

.PHONY: build
build:
	@sudo nixos-rebuild $(FLAGS) build

.PHONY: switch
switch:
	@sudo nixos-rebuild $(FLAGS) switch

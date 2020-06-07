FLAGS := -I "packages=$$(pwd)/packages" \
		 -I "modules=$$(pwd)/modules" \
		 -I "config=$$(pwd)/config" \
		 $(FLAGS)

.PHONY: build
build:
	@home-manager $(FLAGS) build

.PHONY: switch
switch:
	@home-manager $(FLAGS) switch

FLAGS := -I "bin=$$(pwd)/bin" \
		 -I "config=$$(pwd)/config" \
		 -I "modules=$$(pwd)/modules" \
		 -I "packages=$$(pwd)/packages" \
		 $(FLAGS)

.PHONY: build
build:
	@home-manager $(FLAGS) build

.PHONY: switch
switch:
	@home-manager $(FLAGS) switch

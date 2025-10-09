{ inputs, lib, ... }:

with lib;
let
  mylib = makeExtensible (self: {
    attrs = import ./attrs.nix { inherit self lib; };
    extra-types = import ./extra-types.nix { inherit self lib; };
    generators = import ./generators.nix { inherit self lib; };
    options = import ./options.nix { inherit self lib; };
  });
in
mylib.extend (_: super: foldr (a: b: a // b) { } (attrValues super))

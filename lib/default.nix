{ inputs, lib, ... }:

with lib;
let
  modules = import ./modules.nix {
    inherit lib;
    self.attrs = import ./attrs.nix {
      inherit lib;
      self = { };
    };
  };

  mylib = makeExtensible (self:
    with self;
    modules.mapModules ./. (file: import file { inherit self lib inputs; }));
in
mylib.extend (_: super: foldr (a: b: a // b) { } (attrValues super))

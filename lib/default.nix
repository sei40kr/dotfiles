{ haumea, inputs, lib, ... }:

with lib;
let
  mylib = makeExtensible (
    self:
    let
      libInputs = { inherit self lib inputs; };
      loaded = haumea.lib.load {
        src = ./.;
        inputs = libInputs;
      };
    in
    removeAttrs loaded [ "default" "modules" ]
  );
in
mylib.extend (_: super: foldr (a: b: a // b) { } (attrValues super))

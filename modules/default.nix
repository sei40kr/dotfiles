{ config, inputs, lib, pkgs, ... }:

let
  loader = final: prev: {
    imports = [ (prev final) ];
  };
in
inputs.haumea.lib.load {
  src = ./.;
  inputs = { inherit config inputs lib pkgs; };
  loader = loader;
  transformer = inputs.haumea.lib.transformers.hoistLists "imports" "modules";
}

{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.ocaml;
in
{
  options.modules.dev.ocaml = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enable OCaml development environment.
      '';
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ocaml
      ocamlPackages.findlib
      dune_3
      ocamlformat
      ocamlPackages.ocaml-lsp
    ];
  };
}

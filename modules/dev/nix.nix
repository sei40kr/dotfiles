{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.nix;
in {
  options.modules.dev.nix = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ nixpkgs-fmt nix-linter rnix-lsp ];
  };
}

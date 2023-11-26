{ config, inputs, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (pkgs.stdenv.hostPlatform) system;
  cfg = config.modules.dev.nix;
in
{
  options.modules.dev.nix = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      nix-init
      nix-melt
      nurl
      nixpkgs-fmt
      inputs.nil.packages.${system}.default
    ];
  };
}

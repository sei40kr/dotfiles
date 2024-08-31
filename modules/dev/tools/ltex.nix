{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.tools.ltex;
in
{
  options.modules.dev.tools.ltex = {
    enable = mkEnableOption "LTEX";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ ltex-ls ];
  };
}

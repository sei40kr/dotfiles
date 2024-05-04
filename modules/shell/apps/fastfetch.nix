{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.modules.shell.apps.fastfetch;
in
{
  options.modules.shell.apps.fastfetch = {
    enable = mkEnableOption "fastfetch";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ fastfetch ];
  };
}

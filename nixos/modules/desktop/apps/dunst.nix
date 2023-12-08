{ config, lib, pkgs, ... }:

let
  inherit (lib) mdDoc mkEnableOption mkIf mkOption types;
  inherit (types) package;
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.apps.dunst;
in
{
  options.modules.desktop.apps.dunst = {
    enable = mkEnableOption (mdDoc ''
      Whether to enable Dunst.
    '');

    package = mkOption {
      type = package;
      default = pkgs.dunst;
      description = mdDoc ''
        The package to use for Dunst.
      '';
      visible = false;
    };
  };

  config = mkIf cfg.enable {
    user.packages = [ cfg.package ];

    environment.etc."dunst/dunstrc".source = "${configDir}/dunst/dunstrc";
  };
}

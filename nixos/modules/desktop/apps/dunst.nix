{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.apps.dunst;
in
{
  options.modules.desktop.apps.dunst = {
    enable = mkBoolOpt false;

    package = mkOption {
      default = pkgs.dunst;
      type = types.package;
      visible = false;
    };
  };

  config = mkIf cfg.enable {
    user.packages = [ cfg.package ];

    environment.etc."dunst/dunstrc".source = "${configDir}/dunst/dunstrc";
  };
}

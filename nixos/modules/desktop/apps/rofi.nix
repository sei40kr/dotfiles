{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.apps.rofi;
in
{
  options.modules.desktop.apps.rofi = {
    enable = mkBoolOpt false;

    package = mkOption {
      default = pkgs.rofi;
      type = types.package;
      visible = false;
    };
  };

  config = mkIf cfg.enable {
    user.packages = [ cfg.package ];

    environment.etc."rofi.rasi".source = "${configDir}/rofi/rofi.rasi";
  };
}

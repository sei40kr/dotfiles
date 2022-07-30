{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.i3;

  package = pkgs.i3-gaps;
in
{
  options.modules.desktop.i3 = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ package ];

    environment.etc."i3/config".source = "${configDir}/i3/config";

    services.xserver.windowManager.session = [{
      name = "i3";
      start = ''
        ${package}/bin/i3 &
        waitPID=$!
      '';
    }];

    modules.desktop.apps.dunst.enable = true;
    modules.desktop.apps.rofi.enable = true;
  };
}

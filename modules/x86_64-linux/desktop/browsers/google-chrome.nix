{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.google-chrome;
in {
  options.modules.desktop.browsers.google-chrome = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.google-chrome = {
      enable = true;
      package = pkgs.unstable.google-chrome;
    };
  };
}

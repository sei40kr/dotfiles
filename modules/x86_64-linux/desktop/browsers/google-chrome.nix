{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.google-chrome;
  package = pkgs.unstable.google-chrome.override {
    commandLineArgs = "--disable-gpu ${
        optionalString config.modules.desktop.wayland
        "--enable-features=UseOzonePlatform --ozone-platform=wayland"
      }";
  };
in {
  options.modules.desktop.browsers.google-chrome = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.google-chrome = {
      inherit package;
      enable = true;
    };
  };
}

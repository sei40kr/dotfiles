{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.i18n.inputMethod) package;
  cfg = config.modules.desktop.fcitx5;
in {
  options.modules.desktop.fcitx5 = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc ];
    };

    systemd.user.services.fcitx5-daemon.enable = mkForce false;
  };
}

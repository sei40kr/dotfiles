{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.i18n.japanese;

  fcitx5Package = config.i18n.inputMethod.package;
in
{
  config = mkIf (cfg.enable && config.modules.desktop.enable) {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc ];
    };

    # This still works because Fcitx5 can simulate IBus protocol
    environment.variables.GLFW_IM_MODULE = "ibus";

    # Set the same environment variables as environment.variables to
    # environment.sessionVariables or we can't export them to apps in some
    # cases. See NixOS/nixpkgs#129442.
    environment.sessionVariables = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };

    systemd.user.services.fcitx5-daemon = {
      description = "Fcitx5 input method editor";
      documentation = [ "https://fcitx-im.org" ];
      partOf = [ "autostart.target" ];
      wantedBy = [ "autostart.target" ];
      aliases = [ "input-method.service" ];
      serviceConfig = {
        ExecStart = "${fcitx5Package}/bin/fcitx5";
      };
    };
  };
}

{ config, lib, options, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.apps.thunar;
in {
  options.modules.desktop.apps.thunar = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    enableDaemon = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    modules.desktop.gtk.enable = mkForce true;

    my.packages = with pkgs; [ xfce.thunar ];
    my.home.systemd.user.services.thunar = mkIf cfg.enableDaemon {
      Unit = {
        Description = "Thunar file manager";
        Documentation = "man:Thunar(1)";
      };
      Service = {
        Type = "dbus";
        ExecStart = "${pkgs.xfce.thunar}/bin/Thunar --daemon";
        BusName = "org.xfce.FileManager";
        KillMode = "process";
      };
    };
  };
}

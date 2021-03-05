{ config, home-manager, lib, pkgs, ... }:

with lib;
let package = pkgs.gnome3.gvfs;
in {
  options.modules.desktop.gvfs.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.gvfs.enable {
    user.packages = [ package ];
    env.GIO_EXTRA_MODULES = [ "${package}/lib/gio/modules" ];
    home-manager.users.${config.user.name}.systemd.user.services = {
      gvfs-afc-volume-monitor = {
        Unit.Description =
          "Virtual filesystem service - Apple File Conduit monitor";
        Service = {
          ExecStart = "${package}/libexec/gvfs-afc-volume-monitor";
          Type = "dbus";
          BusName = "org.gtk.vfs.AfcVolumeMonitor";
        };
      };
      gvfs-daemon = {
        Unit.Description = "Virtual filesystem service";
        Service = {
          ExecStart = "${package}/libexec/gvfsd";
          Type = "dbus";
          BusName = "org.gtk.vfs.Daemon";
        };
      };
      gvfs-gphoto2-volume-monitor = {
        Unit.Description =
          "Virtual filesystem service - digital camera monitor";
        Service = {
          ExecStart = "${package}/libexec/gvfs-gphoto2-volume-monitor";
          Type = "dbus";
          BusName = "org.gtk.vfs.GPhoto2VolumeMonitor";
        };
      };
      gvfs-metadata = {
        Unit.Description = "Virtual filesystem metadata service";
        Service = {
          ExecStart = "${package}/libexec/gvfsd-metadata";
          Type = "dbus";
          BusName = "org.gtk.vfs.Metadata";
        };
      };
      gvfs-mtp-volume-monitor = {
        Unit.Description =
          "Virtual filesystem service - Media Transfer Protocol monitor";
        Service = {
          ExecStart = "${package}/libexec/gvfs-mtp-volume-monitor";
          Type = "dbus";
          BusName = "org.gtk.vfs.MTPVolumeMonitor";
        };
      };
      gvfs-udisks2-volume-monitor = {
        Unit.Description = "Virtual filesystem service - disk device monitor";
        Service = {
          ExecStart = "${package}/libexec/gvfs-udisks2-volume-monitor";
          Type = "dbus";
          BusName = "org.gtk.vfs.UDisks2VolumeMonitor";
        };
      };
    };
    services = {
      # TODO Use user D-Bus module
      dbus = {
        enable = true;
        packages = [ package ];
      };
      udev.packages = with pkgs; [ libmtp.bin ];
    };
  };
}

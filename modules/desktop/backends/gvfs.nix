{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.backends.gvfs.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.backends.gvfs.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ gnome3.gvfs ];
    };

    my.packages = with pkgs; [ gnome3.gvfs ];
    services.udev.packages = with pkgs; [ libmtp.bin ];
    my.env.GIO_EXTRA_MODULES = [ "${pkgs.gnome3.gvfs}/lib/gio/modules" ];
    my.home.systemd.user.services = {
      gvfs-afc-volume-monitor = {
        Unit.Description =
          "Virtual filesystem service - Apple File Conduit monitor";
        Service = {
          ExecStart = "${pkgs.gnome3.gvfs}/libexec/gvfs-afc-volume-monitor";
          Type = "dbus";
          BusName = "org.gtk.vfs.AfcVolumeMonitor";
        };
      };
      gvfs-daemon = {
        Unit.Description = "Virtual filesystem service";
        Service = {
          ExecStart = "${pkgs.gnome3.gvfs}/libexec/gvfsd";
          Type = "dbus";
          BusName = "org.gtk.vfs.Daemon";
        };
      };
      gvfs-gphoto2-volume-monitor = {
        Unit.Description =
          "Virtual filesystem service - digital camera monitor";
        Service = {
          ExecStart = "${pkgs.gnome3.gvfs}/libexec/gvfs-gphoto2-volume-monitor";
          Type = "dbus";
          BusName = "org.gtk.vfs.GPhoto2VolumeMonitor";
        };
      };
      gvfs-metadata = {
        Unit.Description = "Virtual filesystem metadata service";
        Service = {
          ExecStart = "${pkgs.gnome3.gvfs}/libexec/gvfsd-metadata";
          Type = "dbus";
          BusName = "org.gtk.vfs.Metadata";
        };
      };
      gvfs-mtp-volume-monitor = {
        Unit.Description =
          "Virtual filesystem service - Media Transfer Protocol monitor";
        Service = {
          ExecStart = "${pkgs.gnome3.gvfs}/libexec/gvfs-mtp-volume-monitor";
          Type = "dbus";
          BusName = "org.gtk.vfs.MTPVolumeMonitor";
        };
      };
      gvfs-udisks2-volume-monitor = {
        Unit.Description = "Virtual filesystem service - disk device monitor";
        Service = {
          ExecStart = "${pkgs.gnome3.gvfs}/libexec/gvfs-udisks2-volume-monitor";
          Type = "dbus";
          BusName = "org.gtk.vfs.UDisks2VolumeMonitor";
        };
      };
    };
  };
}

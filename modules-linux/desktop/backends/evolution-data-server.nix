{ config, home-manager, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.backends.evolutionDataServer.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.backends.evolutionDataServer.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.evolution-data-server ];
      };
      glibNetworking.enable = mkForce true;
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.evolution-data-server ];
      };
    };

    user.packages = with pkgs; [ gnome3.evolution-data-server ];
    home-manager.users.${config.user.name}.systemd.user.services = {
      evolution-addressbook-factory = {
        Unit.Description = "Evolution address book service";
        Service = {
          Type = "dbus";
          BusName = "org.gnome.evolution.dataserver.AddressBook10";
          ExecStart =
            "${pkgs.evolution-data-server}/libexec/evolution-addressbook-factory";
        };
      };

      evolution-calendar-factory = {
        Unit.Description = "Evolution calendar service";
        Service = {
          Type = "dbus";
          BusName = "org.gnome.evolution.dataserver.Calendar8";
          ExecStart =
            "${pkgs.evolution-data-server}/libexec/evolution-calendar-factory";
        };
      };

      evolution-source-registry = {
        Unit.Description = "Evolution source registry";
        Service = {
          Type = "dbus";
          BusName = "org.gnome.evolution.dataserver.Sources5";
          ExecStart =
            "${pkgs.evolution-data-server}/libexec/evolution-source-registry";
        };
      };

      evolution-user-prompter = {
        Unit.Description = "Evolution user prompter";
        Service = {
          Type = "dbus";
          BusName = "org.gnome.evolution.dataserver.UserPrompter0";
          ExecStart =
            "${pkgs.evolution-data-server}/libexec/evolution-user-prompter";
        };
      };
    };
  };
}

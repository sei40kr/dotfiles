{ config, lib, pkgs, ... }:

with lib;
let
  package = with pkgs;
    flexget.overrideAttrs (oldAttrs: {
      propagatedBuildInputs = oldAttrs.propagatedBuildInputs
        ++ [ python3Packages.deluge-client ];
    });
  home = config.users.users."${config.my.userName}".home;
  downloadDir = "${home}/Downloads";
  config_yml =
    import <secrets/config/flexget/config_yml.nix> { inherit downloadDir; };
in {
  options.modules.services.flexget.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.flexget.enable {
    my.packages = [ package ];
    my.home.xdg.configFile."flexget/config.yml".text =
      generators.toYAML { } config_yml;
    my.home.systemd.user.services.flexget = {
      Unit = {
        Description = "FlexGet Daemon";
        X-Restart-Triggers = [ "%h/.config/flexget/config.yml" ];
      };
      Service = {
        ExecStart = "${package}/bin/flexget daemon start";
        ExecStop = "${package}/bin/flexget daemon stop";
        ExecReload = "${package}/bin/flexget daemon reload";
        Restart = "on-failure";
        PrivateTmp = true;
      };
    };
  };
}

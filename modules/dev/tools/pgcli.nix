{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.pgcli.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.pgcli.enable {
    my.packages = with pkgs; [ pgcli ];
    my.home.xdg.configFile."pgcli/config".source = <config/pgcli/config>;
  };
}

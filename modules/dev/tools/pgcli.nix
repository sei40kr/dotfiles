{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.dev.tools.pgcli.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.pgcli.enable {
    user.packages = with pkgs; [ pgcli ];
    home.configFile."pgcli/config".source = "${configDir}/pgcli/config";
  };
}

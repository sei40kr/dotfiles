{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.dev.sql;
in {
  options.modules.dev.sql.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ mycli pgcli ];
    home = {
      file.".myclirc".source = "${configDir}/mycli/myclirc";
      configFile."pgcli/config".source = "${configDir}/pgcli/config";
    };
  };
}

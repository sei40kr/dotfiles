{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.dev.sql;
in
{
  options.modules.dev.sql = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ litecli mycli pgcli ];

    home.configFile."litecli/config".source = "${configDir}/litecli/config";

    home.file.".myclirc".source = "${configDir}/mycli/myclirc";

    home.configFile."pgcli/config".source = "${configDir}/pgcli/config";
  };
}

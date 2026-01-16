{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.sql;
in
{
  options.modules.dev.lang.sql = {
    enable = mkEnableOption "SQL development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      litecli
      mycli
      pgcli
    ];

    xdg.configFile."litecli/config".source = ../../config/litecli/config;

    home.file.".myclirc".source = ../../config/mycli/myclirc;

    xdg.configFile."pgcli/config".source = ../../config/pgcli/config;
  };
}

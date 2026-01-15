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

    xdg.configFile."litecli/config".source = ./litecli-config;

    home.file.".myclirc".source = ./myclirc;

    xdg.configFile."pgcli/config".source = ./pgclirc;
  };
}

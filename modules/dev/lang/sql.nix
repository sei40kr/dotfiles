{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.dev.lang.sql;
in
{
  options.modules.dev.lang.sql = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      litecli
      mycli
      pgcli
    ];

    home.configFile."litecli/config".source = ../../../config/litecli/config;

    home.file.".myclirc".source = ../../../config/mycli/myclirc;

    home.configFile."pgcli/config".source = ../../../config/pgcli/config;
  };
}

{ config, lib, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.i18n.japanese;
in
{
  options.modules.i18n.japanese = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    i18n.extraLocaleSettings = {
      LC_CTYPE = "ja_JP.UTF-8";
    };
  };
}

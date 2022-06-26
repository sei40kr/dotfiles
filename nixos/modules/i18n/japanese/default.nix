{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.i18n.japanese;
in
{
  options.modules.i18n.japanese = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    i18n.extraLocaleSettings = {
      LC_CTYPE = "ja_JP.UTF-8";
    };
  };
}

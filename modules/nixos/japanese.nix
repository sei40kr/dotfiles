{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.i18n.japanese;
in
{
  options.modules.i18n.japanese = {
    enable = mkEnableOption "Japanese language support";
  };

  config = mkIf cfg.enable {
    i18n.extraLocaleSettings = {
      LC_CTYPE = "ja_JP.UTF-8";
    };
  };
}

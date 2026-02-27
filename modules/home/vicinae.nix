{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.vicinae;
in
{
  options.modules.desktop.apps.vicinae = {
    enable = mkEnableOption "Vicinae";
  };

  config = mkIf cfg.enable {
    programs.vicinae = {
      enable = true;
      systemd.enable = true;
    };
  };
}

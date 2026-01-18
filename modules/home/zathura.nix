{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.media.zathura;
in
{
  options.modules.desktop.media.zathura = {
    enable = mkEnableOption "Zathura";
  };

  config = mkIf cfg.enable {
    programs.zathura.enable = true;
  };
}

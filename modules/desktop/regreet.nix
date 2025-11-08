{
  config,
  lib,
  ...
}:

let
  inherit (builtins) floor toString;
  inherit (lib)
    mkEnableOption
    mkIf
    ;

  cfg = config.modules.desktop.regreet;
  deCfg = config.modules.desktop.de;
  gtkCfg = config.modules.desktop.gtk;

  backgroundFit =
    if deCfg.background.image == null then
      "Contain"
    else if deCfg.background.image.mode == "fill" then
      "Cover"
    else if deCfg.background.image.mode == "fit" then
      "Contain"
    else if deCfg.background.image.mode == "stretch" then
      "Fill"
    else
      throw "Unsupported background mode '${deCfg.background.image.mode}' for regreet. Supported modes: fill, fit, stretch";
in
{
  options.modules.desktop.regreet = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    programs.regreet = {
      enable = true;
      settings = mkIf (deCfg.background.image != null) {
        background = {
          path = toString deCfg.blurredBackgroundImage;
          fit = backgroundFit;
        };
      };
      theme = mkIf (gtkCfg.theme != null) {
        inherit (gtkCfg.theme) package name;
      };
      iconTheme = mkIf (gtkCfg.iconTheme != null) {
        inherit (gtkCfg.iconTheme) package name;
      };
      font = {
        inherit (deCfg.defaultFonts.ui) package name;
        size = floor deCfg.defaultFonts.ui.size;
      };
    };
  };
}

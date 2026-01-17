{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (builtins) floor toString;
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.desktop.regreet;
  deCfg = config.modules.desktop.de;

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
    enable = mkEnableOption "ReGreet greeter";
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
      # TODO: Add regreet module options for theme and iconTheme
      # theme = {
      #   package = ...;
      #   name = ...;
      # };
      # iconTheme = {
      #   package = ...;
      #   name = ...;
      # };
      font = {
        inherit (deCfg.defaultFonts.ui) package name;
        size = floor deCfg.defaultFonts.ui.size;
      };
    };
  };
}

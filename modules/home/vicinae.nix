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
      settings = {
        theme.name = "tokyo-night";
      };
      themes.tokyo-night = {
        meta = {
          version = 1;
          name = "Tokyo Night";
          variant = "dark";
          inherits = "vicinae-dark";
        };
        colors = {
          core = {
            background = "#292e42";
            foreground = "#c0caf5";
            secondary_background = "#0c0e14";
            border = "#3b4261";
            accent = "#7aa2f7";
          };
          accents = {
            blue = "#7aa2f7";
            green = "#9ece6a";
            magenta = "#ff007c";
            orange = "#ff9e64";
            purple = "#9d7cd8";
            red = "#f7768e";
            yellow = "#e0af68";
            cyan = "#7dcfff";
          };
        };
      };
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.vicinae;

  extensionsSrc =
    pkgs.fetchFromGitHub {
      owner = "vicinaehq";
      repo = "extensions";
      rev = "cf30b80f619282d45b1748eb76e784a4f875bb01";
      sha256 = "sha256-KwNv+THKbNUey10q26NZPDMSzYTObRHaSDr81QP9CPY=";
    }
    + "/extensions";
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
      extensions =
        map
          (
            name:
            config.lib.vicinae.mkExtension {
              inherit name;
              src = extensionsSrc + "/${name}";
            }
          )
          [
            "bluetooth"
            "github"
            "niri"
            "nix"
            "stocks"
          ];
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

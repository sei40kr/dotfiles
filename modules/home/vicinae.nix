{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.vicinae;
  termColorschemesCfg = config.modules.term.colorschemes;

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
        theme.name = termColorschemesCfg.themes.${termColorschemesCfg.active}.vicinae;
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
    };
  };
}

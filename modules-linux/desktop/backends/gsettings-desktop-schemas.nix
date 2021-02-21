{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.backends.gsettingsDesktopSchemas;
in {
  options.modules.desktop.backends.gsettingsDesktopSchemas = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    packages = mkOption {
      type = with types; listOf package;
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ gsettings-desktop-schemas ];
    env.XDG_DATA_DIRS = map (p: "${p}/share/gsettings-schemas/${p.name}")
      (cfg.packages ++ [ pkgs.gsettings-desktop-schemas ]);
  };
}

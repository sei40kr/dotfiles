{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mdDoc
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  inherit (lib.strings) sanitizeDerivationName;
  inherit (types)
    either
    enum
    int
    nullOr
    path
    str
    submodule
    ;
  inherit (inputs.self.lib.extraTypes) fontType;

  cfg = config.modules.desktop.de;

  backgroundImageType = submodule {
    options = {
      path = mkOption {
        type = either str path;
        example = "/path/to/background.png";
        description = mdDoc ''
          The path to the background image.
        '';
      };

      mode = mkOption {
        type = enum [
          "stretch"
          "fill"
          "fit"
          "center"
          "tile"
        ];
        default = "fill";
        description = mdDoc ''
          The mode to use for the background image.
        '';
      };
    };
  };

  blurImage =
    path:
    pkgs.runCommand (sanitizeDerivationName "blurred-${builtins.baseNameOf path}")
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
        buildInputs = with pkgs; [ imagemagick ];
      }
      ''
        magick ${path} -blur 0x60 -modulate 55,100,100 $out
      '';
in
{
  options.modules.desktop.de = {
    enable =
      (mkEnableOption (mdDoc ''
        Set this to true when you want to use a desktop environment.
      ''))
      // {
        visible = false;
      };

    wayland =
      (mkEnableOption (mdDoc ''
        Set this to true when you want to use a Wayland session.
      ''))
      // {
        default = false;
      };

    defaultFonts = {
      ui = mkOption {
        type = fontType;
        default = {
          package = pkgs.cantarell-fonts;
          name = "Cantarell";
          size = 11;
        };
        description = mdDoc ''
          The default font to use for UI elements.
        '';
      };

      fixed = mkOption {
        type = fontType;
        default = {
          package = pkgs.source-code-pro;
          name = "Source Code Pro";
          size = 10;
        };
        description = mdDoc ''
          The default font to use for fixed width text.
        '';
      };

      document = mkOption {
        type = fontType;
        default = {
          package = pkgs.cantarell-fonts;
          name = "Cantarell";
          size = 11;
        };
        description = mdDoc ''
          The default font to use for documents.
        '';
      };
    };

    background = {
      image = mkOption {
        type = nullOr backgroundImageType;
        default = null;
        example = {
          path = "/path/to/background.png";
          mode = "fill";
        };
        description = mdDoc ''
          The background image to use.
        '';
      };

      color = mkOption {
        type = str;
        default = "#404040";
        description = mdDoc ''
          The background color to use.
        '';
      };
    };

    blurredBackgroundImage = mkOption {
      type = nullOr path;
      visible = false;
      default = if cfg.background.image != null then blurImage cfg.background.image.path else null;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (mkIf (!cfg.wayland) xclip)
      (mkIf cfg.wayland wl-clipboard)

      (makeDesktopItem {
        name = "shutdown";
        desktopName = "Shutdown";
        exec = "systemctl poweroff";
        icon = "shutdown";
        terminal = false;
        categories = [ "System" ];
        comment = "Power off the system";
      })
      (makeDesktopItem {
        name = "reboot";
        desktopName = "Reboot";
        exec = "systemctl reboot";
        icon = "reboot";
        terminal = false;
        categories = [ "System" ];
        comment = "Restart the system";
      })
    ];

    fonts.packages = [
      (mkIf (cfg.defaultFonts.ui.package != null) cfg.defaultFonts.ui.package)
      (mkIf (cfg.defaultFonts.fixed.package != null) cfg.defaultFonts.fixed.package)
      (mkIf (cfg.defaultFonts.document.package != null) cfg.defaultFonts.document.package)
    ];

    environment.sessionVariables.NIXOS_OZONE_WL = mkIf cfg.wayland "1";
  };
}

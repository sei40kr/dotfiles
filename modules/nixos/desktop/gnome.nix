{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.gnome;
in {
  options.modules.desktop.gnome = with types; {
    enable = mkBoolOpt false;
    enabledExtensions = mkOpt (listOf str) [ ];
  };

  config = mkIf cfg.enable {
    services = {
      xserver = {
        enable = true;
        displayManager.gdm.enable = true;
        desktopManager.gnome.enable = true;
      };
      gnome.core-utilities.enable = false;
    };
    environment.gnome.excludePackages = with pkgs; [ gnome.gnome-terminal ];
    networking.networkmanager.packages = with pkgs;
      [ gnome.networkmanager-openvpn ];

    user.packages = with pkgs; [
      # GNOME core utilities
      gnome.gnome-calculator
      gnome.gnome-screenshot

      # GNOME core developer tools
      gnome.dconf-editor
      gnome.gnome-tweaks
    ];

    modules = {
      desktop = {
        fcitx5.enable = true;
        dconf = {
          enable = true;
          settings = {
            "org/gnome/desktop/interface".enable-animations = false;
            "org/gnome/desktop/peripherals/keyboard" = {
              delay = 150;
              repeat-interval = 30;
            };
            "org/gnome/desktop/peripherals/mouse".accel-profile = "flat";
            "org/gnome/shell".enabled-extensions =
              mkIf (cfg.enabledExtensions != [ ]) cfg.enabledExtensions;
          };
        };
        fontconfig.enable = true;
        gtk = {
          enable = true;
          font = {
            package = pkgs.roboto;
            name = "Roboto";
            size = 10;
          };
          theme = {
            theme.name = "Adwaita-dark";
            iconTheme = {
              package = pkgs.papirus-icon-theme;
              name = "Papirus";
            };
          };
        };
        qt = {
          enable = true;
          theme = {
            platformTheme = "gnome";
            style = {
              package = pkgs.adwaita-qt;
              name = "adwaita-dark";
            };
          };
        };
      };
      term = {
        font = {
          name = "JetBrains Mono";
          size = 13;
        };
        theme.colorscheme = "doom-one";
      };
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gtk;

  formatGtk2Option =
    n: v:
    let
      v' =
        if isBool v then
          (if v then "true" else "false")
        else if isString v then
          ''"${v}"''
        else
          toString v;
    in
    "${n} = ${v'}";
  toGtkIni = lib.generators.toINI {
    mkKeyValue =
      n: v:
      let
        v' = if isBool v then (if v then "true" else "false") else toString v;
      in
      "${n}=${v'}";
  };
in
{
  options.modules.desktop.gtk = with types; {
    enable = mkBoolOpt false;

    font = mkOpt (nullOr attrs) null;

    iconTheme = mkOpt (nullOr attrs) null;

    theme = mkOpt (nullOr attrs) null;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (mkIf (cfg.theme != null) cfg.theme.package)
      (mkIf (cfg.iconTheme != null) cfg.iconTheme.package)
    ];

    fonts.packages = with pkgs; [ (mkIf (cfg.font != null) cfg.font.package) ];

    home.configFile = {
      "gtk-2.0/gtkrc".text = concatStringsSep "\n" (
        mapAttrsToList formatGtk2Option (
          (optionalAttrs (cfg.font != null) { gtk-font-name = "${cfg.font.name} ${toString cfg.font.size}"; })
          // (optionalAttrs (cfg.theme != null) { gtk-theme-name = cfg.theme.name; })
          // (optionalAttrs (cfg.iconTheme != null) { gtk-icon-theme-name = cfg.iconTheme.name; })
        )
      );
      "gtk-3.0/settings.ini".text = toGtkIni {
        Settings =
          {
            gtk-enable-primary-paste = false;
          }
          // (optionalAttrs (cfg.font != null) {
            gtk-font-name = "${cfg.font.name} ${toString cfg.font.size}";
          })
          // (optionalAttrs (cfg.theme != null) { gtk-theme-name = cfg.theme.name; })
          // (optionalAttrs (cfg.iconTheme != null) { gtk-icon-theme-name = cfg.iconTheme.name; });
      };
    };

    environment.sessionVariables = {
      GTK2_RC_FILES = "${config.home-manager.users.${config.user.name}.xdg.configHome}/gtk-2.0/gtkrc";
    };

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "org/gnome/desktop/interface" = {
          icon-theme = mkIf (cfg.iconTheme != null) cfg.iconTheme.name;
          gtk-theme = mkIf (cfg.theme != null) cfg.theme.name;
        };
      };
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf types;
  inherit (types)
    nullOr
    package
    str
    submodule
    ;
  inherit (lib.my) mkOpt;

  qtCfg = config.modules.desktop.qt;
  cfg = qtCfg.kvantum;

  themeType = submodule {
    options = {
      package = mkOpt package null;
      dir = mkOpt str null;
      name = mkOpt str null;
    };
  };
in
{
  options.modules.desktop.qt.kvantum = {
    theme = mkOpt (nullOr themeType) null;
  };

  config = mkIf (qtCfg.enable && cfg.theme != null) {
    user.packages = with pkgs; [ libsForQt5.qtstyleplugin-kvantum ];

    environment.variables.QT_STYLE_OVERRIDE = "kvantum";

    home.configFile."Kvantum/${cfg.theme.dir}".source =
      "${cfg.theme.package}/share/Kvantum/${cfg.theme.dir}";
    home.configFile."Kvantum/kvantum.kvconfig".text = ''
      [General]
      theme=${cfg.theme.name}
    '';
  };
}

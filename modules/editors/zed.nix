{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins)
    floor
    fromJSON
    readFile
    toJSON
    ;
  inherit (lib) mkIf mkEnableOption recursiveUpdate;
  inherit (config.dotfiles) configDir;
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.zed;
  termFont = config.modules.term.font;

  settings = recursiveUpdate (fromJSON (readFile "${configDir}/zed/settings.json")) {
    buffer_font_family = editorsCfg.fonts.code.name;
    buffer_font_size = floor (editorsCfg.fonts.code.size / 72.0 * 96.0);
    ui_font_family = editorsCfg.fonts.ui.name;
    ui_font_size = floor (editorsCfg.fonts.ui.size / 72.0 * 96.0);
    terminal = {
      font_size = floor (termFont.size / 72.0 * 96.0);
      font_family = termFont.name;
    };
  };
in
{
  options.modules.editors.zed = {
    enable = mkEnableOption "Zed";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ zed-editor ];

    home.configFile."zed/settings.json".text = toJSON settings;
    home.configFile."zed/keymap.json".source = "${configDir}/zed/keymap.jsonc";

    fonts.packages = with pkgs; [ nerd-fonts.symbols-only ];
  };
}

{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkIf mkEnableOption;

  cfg = config.modules.shell.apps.fastfetch;
in
{
  options.modules.shell.apps.fastfetch = {
    enable = mkEnableOption "fastfetch";
  };

  config = mkIf cfg.enable {
    programs.fastfetch = {
      enable = true;
      settings = {
        logo = {
          type = "kitty-direct";
          source = ../../config/fastfetch/logo.png;
          # FIXME: kitty-direct type cannot preserve aspect ratio of the image,
          #  so setting both width and height.
          #  Remove the height once the issue is fixed.
          #  https://github.com/fastfetch-cli/fastfetch/wiki/Logo-options#kitty-direct
          width = 20;
          height = 9;
        };
        modules = [
          "title"
          "separator"
          "os"
          "host"
          "kernel"
          "uptime"
          "packages"
          "shell"
          "display"
          "de"
          "wm"
          "wmtheme"
          "theme"
          "icons"
          "font"
          "cursor"
          "terminal"
          "terminalfont"
          "cpu"
          "gpu"
          "memory"
          "swap"
          "disk"
          "localip"
          "battery"
          "poweradapter"
          "locale"
          "break"
          "colors"
        ];
      };
    };
  };
}

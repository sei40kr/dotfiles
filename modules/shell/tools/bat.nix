{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.tools.bat;
in {
  options.modules.shell.tools.bat = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    theme = mkOption {
      type = types.str;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ bat ];
    my.aliases.cat = "bat --theme=${escapeShellArg cfg.theme}";
  };
}

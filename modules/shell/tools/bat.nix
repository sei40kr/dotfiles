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
    user.packages = with pkgs; [ bat ];
    modules.shell.zsh.aliases.cat = "bat --theme=${cfg.theme}";
  };
}

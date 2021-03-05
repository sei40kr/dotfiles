{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.bat;
in {
  options.modules.shell.bat = with types; {
    enable = mkBoolOpt false;
    theme = mkOpt str null;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ bat ];
    home-manager.users.${config.user.name}.programs.bat = {
      enable = true;
      config.theme = cfg.theme;
    };
    modules.shell.zsh.aliases.cat = "bat";
  };
}

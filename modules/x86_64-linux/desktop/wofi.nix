{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.wofi;
in {
  options.modules.desktop.wofi = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ wofi ];
    home.configFile."wofi/config".text = ''
      hide_scroll=true
      insensitive=true
      no_actions=true
      term=${config.modules.desktop.term.terminal}
      width=640
    '';
  };
}

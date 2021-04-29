{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.termite;
in {
  options.modules.desktop.term.termite = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.termite = {
      enable = true;
      font = "Monospace 12";
    };
    modules.desktop.term.terminal = "${pkgs.termite}/bin/termite";
  };
}

{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.idea;
in {
  options.modules.editors.idea = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ jetbrains.idea-ultimate ];
    modules.editors = {
      fonts.enable = true;
      ideavim.enable = true;
    };
  };
}

{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.datagrip;
in {
  options.modules.editors.datagrip = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ jetbrains.datagrip ];
    modules.editors = {
      fonts.enable = true;
      ideavim.enable = true;
    };
  };
}

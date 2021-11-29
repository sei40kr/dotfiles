{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.dataspell;
in {
  options.modules.editors.dataspell = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ my.dataspell ];

    modules.editors = {
      fonts.enable = true;
      ideavim.enable = true;
    };
  };
}

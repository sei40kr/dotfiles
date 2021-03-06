{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.r;
  package = with pkgs;
    rWrapper.override {
      packages = [ rPackages.languageserver rPackages.lintr ];
    };
in {
  options.modules.dev.r = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ package ];
    home.file = {
      ".Renviron".text = ''
        R_LIBS=${package}/library
      '';
      ".Rprofile".text = ''
        options(repos='https://cran.ism.ac.jp');
      '';
    };
    modules.shell.aliases = { R = "R -q --no-save --no-restore-data"; };
  };
}

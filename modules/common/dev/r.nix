{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.r;
  rPackages = with pkgs; [
    rPackages.languageserver
    rPackages.lintr
    (mkIf cfg.jupyter.enable rPackages.IRkernel)
  ];
  package = pkgs.rWrapper.override { packages = rPackages; };
in {
  options.modules.dev.r = {
    enable = mkBoolOpt false;
    jupyter.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ package ];
    home.file = {
      ".Renviron".text = ''
        R_LIBS=${pkgs.R}/library
      '';
      ".Rprofile".text = ''
        options(repos='https://cran.ism.ac.jp');
      '';
    };
    modules.shell.zsh.aliases.R = "R -q --no-save --no-restore-data";
  };
}

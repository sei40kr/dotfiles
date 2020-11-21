{ config, lib, pkgs, ... }:

with lib;
let
  rEnv = pkgs.rWrapper.override {
    packages = with pkgs.rPackages;
      optionals config.modules.dev.editors.tools.enable [
        languageserver
        lintr
      ];
  };
in {
  options.modules.dev.r.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.r.enable {
    my.packages = with pkgs; [ rEnv ];
    my.aliases.R = "R -q --no-save --no-restore-data";
    my.home.home.file = {
      ".Renviron".text = ''
        R_LIBS=${pkgs.R}/library
      '';
      ".Rprofile".text = ''
        options(repos='https://cran.ism.ac.jp');
      '';
    };
  };
}

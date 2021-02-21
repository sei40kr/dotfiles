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
    user.packages = with pkgs; [ rEnv ];
    modules.shell.zsh.aliases.R = "R -q --no-save --no-restore-data";
    home.file = {
      ".Renviron".text = ''
        R_LIBS=${pkgs.R}/library
      '';
      ".Rprofile".text = ''
        options(repos='https://cran.ism.ac.jp');
      '';
    };
  };
}

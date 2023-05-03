{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.r;

  package = with pkgs;
    rWrapper.override {
      packages = [ rPackages.languageserver rPackages.lintr rPackages.styler ];
    };
in
{
  options.modules.dev.r = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ package pkgs.python3Packages.radian ];

    home.file = {
      ".Renviron".text = ''
        R_LIBS=${package}/library
      '';
      ".Rprofile".text = ''
        options(repos='https://cran.ism.ac.jp');
      '';
    };

    # Disable completion because does not work well with a narrow terminal
    home.configFile."radian/profile".text = ''
      options(radian.complete_while_typing = FALSE)
    '';

    modules.shell.aliases.R = "radian -q";
  };
}

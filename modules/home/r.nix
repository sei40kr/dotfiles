{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.r;

  rPackage =
    with pkgs;
    rWrapper.override {
      packages = [
        rPackages.languageserver
        rPackages.lintr
        rPackages.styler
      ];
    };
in
{
  options.modules.dev.lang.r = {
    enable = mkEnableOption "R development environment";
  };

  config = mkIf cfg.enable {
    home.packages = [
      rPackage
      pkgs.radian
    ];

    home.file.".Renviron".text = ''
      R_LIBS=${rPackage}/library
    '';
    home.file.".Rprofile".text = ''
      options(repos='https://cran.ism.ac.jp');
    '';

    # Disable completion because does not work well with a narrow terminal
    xdg.configFile."radian/profile".text = ''
      options(radian.complete_while_typing = FALSE)
    '';

    home.shellAliases.R = "radian -q";

    modules.editors.lspServers.r_language_server = rec {
      package = pkgs.rPackages.languageserver;
      command = "${package}/bin/R";
      args = [
        "--no-echo"
        "-e"
        "languageserver::run()"
      ];
      filetypes = [
        "r"
        "rmd"
        "quarto"
      ];
      rootMarkers = [ ".git" ];
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.dev.lang.r;

  package =
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
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [
      package
      pkgs.radian
    ];

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

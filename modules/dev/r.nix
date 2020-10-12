{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.r.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.r.enable {
    modules.dev.editors.tools.packages = with pkgs.rPackages; [
      languageserver
      lintr
    ];

    my.packages = with pkgs; [ R ];
    my.aliases.R = "R -q --no-save --no-restore-data";
    my.home.home.file = {
      ".Renviron".text = ''
        R_LIBS=${escapeShellArg "${pkgs.R}/library"}
      '';
      ".Rprofile".text = ''
        options(repos='https://cran.ism.ac.jp');
      '';
    };
  };
}

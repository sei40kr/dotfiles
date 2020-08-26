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
    my.aliases.R = "R --no-save --no-restore-data -q";
  };
}

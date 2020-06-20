{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.r.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.r.enable {
    my.packages = with pkgs; [ R ];
    my.zsh.aliases.R = "R --no-save --no-restore-data -q";
  };
}

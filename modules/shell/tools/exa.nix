{ config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.tools.exa.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.exa.enable {
    my.packages = with pkgs; [ exa ];
    my.aliases = {
      ls = "exa -F";
      la = "exa -laFh";
      tree = "exa -T";
    };
  };
}

{ config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.tools.exa.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.exa.enable {
    user.packages = with pkgs; [ exa ];
    modules.shell.zsh.aliases = {
      ls = "exa -F";
      la = "exa -laFh";
      tree = "exa -T";
    };
  };
}

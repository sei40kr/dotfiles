{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.exa.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.exa.enable {
    my = {
      packages = with pkgs; [ exa ];
      zsh.aliases = {
        ls = "exa -F";
        la = "exa -laFh";
        tree = "exa -T";
      };
    };
  };
}

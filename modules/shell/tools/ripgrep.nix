{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.tools.ripgrep.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.ripgrep.enable {
    my.packages = with pkgs; [ ripgrep ];
    my.home.home.file.".ripgreprc".source = <config/ripgrep/ripgreprc>;
  };
}

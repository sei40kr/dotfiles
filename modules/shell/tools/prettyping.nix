{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.tools.prettyping.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.prettyping.enable {
    my.packages = with pkgs; [ prettyping ];
    my.zsh.aliases.ping = "prettyping --nolegend";
  };
}

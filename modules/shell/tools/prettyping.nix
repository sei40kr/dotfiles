{ config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.tools.prettyping.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.prettyping.enable {
    user.packages = with pkgs; [ prettyping ];
    modules.shell.zsh.aliases.ping = "prettyping --nolegend";
  };
}

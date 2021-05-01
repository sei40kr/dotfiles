{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.prettyping;
in {
  options.modules.shell.prettyping = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ prettyping ];
    modules.shell.aliases.ping = "prettyping --nolegend";
  };
}

{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.shell;
in {
  options.modules.dev.shell = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ bash-language-server shellcheck shfmt ];
  };
}

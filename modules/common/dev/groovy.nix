{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.groovy;
in {
  options.modules.dev.groovy = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO groovy-language-server
    user.packages = with pkgs; [ groovy gradle maven ];
  };
}

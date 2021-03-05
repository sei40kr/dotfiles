{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.kotlin;
in {
  options.modules.dev.kotlin = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      kotlin
      gradle
      kotlin-language-server
      ktlint
      maven
    ];
  };
}

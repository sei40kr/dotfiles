{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.groovy;
in
{
  options.modules.dev.lang.groovy = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      groovy
      gradle
      maven
    ];
  };
}

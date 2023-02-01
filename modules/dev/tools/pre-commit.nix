{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.tools.pre-commit;
in
{
  options.modules.dev.tools.pre-commit = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ pre-commit ];
  };
}

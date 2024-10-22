{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.tools.ansible;
in
{
  options.modules.dev.tools.ansible = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ ansible ansible-lint ];
  };
}

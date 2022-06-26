{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.tools.circleci-cli;
in
{
  options.modules.dev.tools.circleci-cli = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ circleci-cli ];
  };
}

{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.tools.aws-cli;
in
{
  options.modules.dev.tools.aws-cli = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ awscli2 ];
  };
}

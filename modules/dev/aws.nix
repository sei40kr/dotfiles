{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.aws;
in
{
  options.modules.dev.aws = {
    enable = mkBoolOpt false;

    cfn.enable = mkBoolOpt false;

    copilot.enable = mkBoolOpt false;

    sam.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      awscli2
      ssm-session-manager-plugin
      (mkIf cfg.cfn.enable python3Packages.cfn-lint)
      (mkIf cfg.cfn.enable cfn-nag)
      (mkIf cfg.copilot.enable copilot-cli)
      (mkIf cfg.sam.enable aws-sam-cli)
    ];
  };
}

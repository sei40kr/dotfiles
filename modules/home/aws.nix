{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf optionals;
  cfg = config.modules.dev.tools.aws;
in
{
  options.modules.dev.tools.aws = {
    enable = mkEnableOption "AWS CLI tools";

    cfn.enable = mkEnableOption "CloudFormation tools (cfn-lint, cfn-nag)";

    copilot.enable = mkEnableOption "AWS Copilot CLI";

    sam.enable = mkEnableOption "AWS SAM CLI";
  };

  config = mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        awscli2
        ssm-session-manager-plugin
      ]
      ++ optionals cfg.cfn.enable [
        python3Packages.cfn-lint
        cfn-nag
      ]
      ++ optionals cfg.copilot.enable [ copilot-cli ];
    # TODO: aws-sam-cli is broken
    # ++ optionals cfg.sam.enable [ aws-sam-cli ]
  };
}

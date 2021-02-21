{ config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.tools.onlineJudgeVerifyHelper.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.onlineJudgeVerifyHelper.enable {
    user.packages = with pkgs.my.python3Packages;
      [ online-judge-verify-helper ];
  };
}

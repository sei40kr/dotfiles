{ config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.tools.onlineJudgeTools.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.onlineJudgeTools.enable {
    my.packages = with pkgs.my.python3Packages; [ online-judge-tools ];
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.solidity;
in
{
  options.modules.dev.lang.solidity = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      solc
      nodePackages.ganache
    ];
  };
}

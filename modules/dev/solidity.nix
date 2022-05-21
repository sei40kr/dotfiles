{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.solidity;
in
{
  options.modules.dev.solidity = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ solc nodePackages.ganache-cli ];
  };
}

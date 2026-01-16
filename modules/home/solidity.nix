{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.solidity;
in
{
  options.modules.dev.lang.solidity = {
    enable = mkEnableOption "Solidity development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      solc
    ];
  };
}

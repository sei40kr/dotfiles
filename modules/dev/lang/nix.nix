{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.nix;
in
{
  options.modules.dev.lang.nix = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      nix-init
      nix-melt
      nurl
      nixfmt-rfc-style
      nil
    ];
  };
}

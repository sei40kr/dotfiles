{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.lang.latex;
in
{
  options.modules.dev.lang.latex = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ texlive.combined.scheme-medium texlab ];
  };
}

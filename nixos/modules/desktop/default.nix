{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) binDir;
  cfg = config.modules.desktop;

  wayland-sensible-terminal = pkgs.runCommandLocal "wayland-sensible-terminal" { } ''
    mkdir -p $out/bin
    cp ${../../../bin}/wayland-sensible-terminal $out/bin
    patchShebangs --build $out/bin/wayland-sensible-terminal
  '';
in
{
  options.modules.desktop = with types; {
    enable = mkBoolOpt false;

    autoRepeat = {
      delay = mkOpt int 200;
      interval = mkOpt int 30;
    };
  };

  config = mkIf cfg.enable {
    user.packages = [ wayland-sensible-terminal ];
  };
}

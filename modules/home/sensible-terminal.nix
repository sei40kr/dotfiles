{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;

  termCfg = config.modules.term;
  cfg = termCfg.sensible;

  sensible-terminal = pkgs.writeShellScriptBin "sensible-terminal" (
    if termCfg.wezterm.enable then
      ''exec wezterm "$@"''
    else if termCfg.kitty.enable then
      ''exec kitty "$@"''
    else
      abort "sensible-terminal: no terminal emulator found"
  );
in
{
  options.modules.term.sensible.enable = mkEnableOption "sensible-terminal";

  config = mkIf cfg.enable {
    home.packages = [ sensible-terminal ];
  };
}

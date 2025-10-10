{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;

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
  options.modules.term.sensible = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable { environment.systemPackages = [ sensible-terminal ]; };
}

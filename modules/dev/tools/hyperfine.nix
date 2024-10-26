{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mdDoc mkEnableOption mkIf;
  cfg = config.modules.dev.tools.hyperfine;
in
{
  options.modules.dev.tools.hyperfine = {
    enable = mkEnableOption (mdDoc ''
      Whether to enable Hyperfine, a command-line benchmarking tool.
    '');
  };

  config = mkIf cfg.enable { user.packages = with pkgs; [ hyperfine ]; };
}

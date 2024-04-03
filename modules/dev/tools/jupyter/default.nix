{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption mkEnableOption types;
  inherit (config.dotfiles) configDir;
  inherit (pkgs) stdenv;
  cfg = config.modules.dev.tools.jupyter;

  kernels = pkgs.jupyter-kernel.create {
    definitions = cfg.kernels;
  };
in
{
  options.modules.dev.tools.jupyter = {
    enable = mkEnableOption "Jupyter";

    kernels = mkOption {
      type = types.attrs;
      default = { };
      visible = false;
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ python3Packages.notebook ];

    home.file.".jupyter/jupyter_console_config.py".source =
      "${configDir}/jupyter/jupyter_console_config.py";

    home.file."Library/Jupyter/kernels" = mkIf stdenv.isDarwin {
      source = "${kernels}/kernels";
    };
    home.dataFile."jupyter/kernels" = mkIf stdenv.isLinux {
      source = "${kernels}/kernels";
    };
  };
}

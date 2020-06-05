{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.tmux.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tmux.enable (let
    tpm =
      builtins.fetchGit { url = "https://github.com/tmux-plugins/tpm.git"; };
  in {
    programs.tmux = {
      baseIndex = 1;
      enable = true;
      extraConfig = ''
        set-environment -g TMUX_PLUGIN_MANAGER_PATH ${
          escapeShellArg tpm.outPath
        }

        source-file ${escapeShellArg <config/tmux/extra.conf>}
      '';
      sensibleOnTop = false;
      terminal = "tmux-256color";
    };
  });
}

{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  inherit (pkgs.stdenv) isDarwin;
  shellCfg = config.modules.shell;
  cfg = shellCfg.zsh;

  zshrc = pkgs.runCommandLocal "zshrc" { } ''
    substitute ${../../../config/zsh/zshrc} $out \
      --subst-var-by oh_my_zsh  ${pkgs.oh-my-zsh} \
      --subst-var-by zsh_prezto ${pkgs.zsh-prezto} \
      --subst-var-by fzf        ${pkgs.fzf} \
      --subst-var-by fd         ${pkgs.fd} \
      --subst-var-by bat        ${pkgs.bat} \
      --subst-var-by exa        ${pkgs.exa}
  '';
in
{
  options.modules.shell.zsh = with types; {
    enable = mkBoolOpt false;

    envInit = mkOpt lines "";
    rcInit = mkOpt lines "";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      zsh

      # zi dependencies
      my.zi
      curl
      file
      git
      subversion

      # Completions
      nix-zsh-completions

      fzf
      starship
      (mkIf stdenv.isDarwin terminal-notifier)
    ];

    environment.etc = {
      zshenv.text = ''
        # Only execute this file once per shell.
        if [[ -n "$__ETC_ZSHENV_SOURCED" ]]; then
          return
        fi
        export __ETC_ZSHENV_SOURCED=1

        if [[ -z "$__NIXOS_SET_ENVIRONMENT_DONE" && -z "$__NIX_DARWIN_SET_ENVIRONMENT_DONE" ]]; then
          . ${config.system.build.setEnvironment}
        fi

        # Read system-wide modifications.
        if [[ -f /etc/zshenv.local ]]; then
          . /etc/zshenv.local
        fi
      '';
      zshrc.text =
        let
          completions = pkgs.buildEnv {
            name = "zsh-completions_system";
            paths = config.environment.systemPackages;
            pathsToLink = [ "/share/zsh" ];
            ignoreCollisions = true;
          };
        in
        ''
          # Only execute this file once per shell.
          if [[ -n "$__ETC_ZSHRC_SOURCED" || -n "$NOSYSZSHRC" ]]; then
            return
          fi
          __ETC_ZSHRC_SOURCED=1

          fpath=(
            "${completions}/share/zsh/site-functions"
            "${completions}/share/zsh/''${ZSH_VERSION}/functions"
            "${completions}/share/zsh/vendor-completions"
            "''${fpath[@]}"
          )

          # Read system-wide modifications.
          if test -f /etc/zshrc.local; then
              . /etc/zshrc.local
          fi
        '';
    };

    home.file = {
      ".zshenv".text = ''. "''${HOME}/.zsh/.zshenv"'';
      ".zsh/.zshenv".text =
        let
          completions = pkgs.buildEnv {
            name = "zsh-completions_user";
            paths = config.users.users.${config.user.name}.packages;
            pathsToLink = [ "/share/zsh" ];
            ignoreCollisions = true;
          };
        in
        ''
          ZDOTDIR="''${HOME}/.zsh"

          . "${
            config.home-manager.users.${config.user.name}.home.profileDirectory
          }/etc/profile.d/hm-session-vars.sh"

          # Don't execute this file when running in a pure nix-shell.
          if [[ -n "$IN_NIX_SHELL" ]]; then
            return
          fi

          fpath=(
            "${completions}/share/zsh/site-functions"
            "${completions}/share/zsh/''${ZSH_VERSION}/functions"
            "${completions}/share/zsh/vendor-completions"
            "''${fpath[@]}"
          )

          ${cfg.envInit}
        '';
      ".zsh/.zshrc".text = ''
        ${optionalString shellCfg.tmux.autoRun ''
          if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIM" && -z "$INSIDE_EMACS" ]]; then
            tmux start-server

            if ! tmux has-session 2>/dev/null; then
              tmux new-session -ds main \; \
                   set-option -t main destroy-unattached off
            fi

            exec tmux attach-session -d
          fi
        ''}

        typeset -U path cdpath fpath manpath

        for profile in "''${(z)NIX_PROFILES}"; do
          fpath+=(
            "''${profile}/share/zsh/site-functions"
            "''${profile}/share/zsh/''${ZSH_VERSION}/functions"
            "''${profile}/share/zsh/vendor-completions"
          )
        done

        HELPDIR="${pkgs.zsh}/share/zsh/''${ZSH_VERSION}/help"

        HISTFILE="''${ZDOTDIR}/.zsh_history"

        ${builtins.readFile zshrc}

        ${cfg.rcInit}

        #
        ## Aliases

        ${concatStringsSep "\n"
        (mapAttrsToList (k: v: "alias ${k}=${escapeShellArg v}")
          shellCfg.aliases)}

        # Disable some features to support TRAMP
        if [[ "$TERM" == dumb ]]; then
          unsetopt ZLE PROMPT_CR PROMPT_SUBST
          unset RPS1 RPROMPT
          PS1='$ '
          PROMPT='$ '
        fi
      '';

      ".zsh/.zi/bin".source = pkgs.my.zi;
    };

    environment.shells =
      [ "/run/current-system/sw/bin/zsh" "${pkgs.zsh}/bin/zsh" ];

    user.shell = pkgs.zsh;
  };
}

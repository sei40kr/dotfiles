{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  shellCfg = config.modules.shell;
  cfg = shellCfg.zsh;

  zshrc = pkgs.runCommandLocal "zshrc" { } ''
    substitute ${../../../config/zsh/zshrc} $out \
      --subst-var-by oh_my_zsh  ${pkgs.oh-my-zsh} \
      --subst-var-by zsh_prezto ${pkgs.zsh-prezto} \
      --subst-var-by fzf        ${pkgs.fzf}
  '';

  starship-nerd-font-symbols-preset = pkgs.runCommandLocal "starship-nerd-font-symbols-preset-${pkgs.starship.version}" { } ''
    mkdir -p $out/etc
    ${pkgs.starship}/bin/starship preset nerd-font-symbols >$out/etc/starship.toml
  '';
in
{
  options.modules.shell.zsh = with types; {
    enable = mkBoolOpt false;

    envInit = mkOpt lines "";
    rcInit = mkOpt lines "";
  };

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      interactiveShellInit =
        let
          completions = pkgs.buildEnv {
            name = "zsh-completions_system";
            paths = config.environment.systemPackages;
            pathsToLink = [ "/share/zsh" ];
            ignoreCollisions = true;
          };
        in
        ''
          fpath=(
            "${completions}/share/zsh/site-functions"
            "${completions}/share/zsh/''${ZSH_VERSION}/functions"
            "${completions}/share/zsh/vendor-completions"
            "''${fpath[@]}"
          )
        '';
      enableCompletion = true;
      enableBashCompletion = false;
    };

    home-manager.users.${config.user.name}.programs.zsh = {
      enable = true;
      dotDir = ".zsh";
      shellAliases = shellCfg.aliases;
      enableCompletion = true;
      completionInit = "";
      history = {
        size = 10000;
        save = 10000;
      };
      initExtraBeforeCompInit =
        let
          completions = pkgs.buildEnv {
            name = "zsh-completions_user";
            paths = config.users.users.${config.user.name}.packages;
            pathsToLink = [ "/share/zsh" ];
            ignoreCollisions = true;
          };
        in
        ''
            fpath=(
              "${completions}/share/zsh/site-functions"
              "${completions}/share/zsh/''${ZSH_VERSION}/functions"
              "${completions}/share/zsh/vendor-completions"
              "''${fpath[@]}"
            )
          ${builtins.readFile zshrc}

          ${cfg.rcInit}

          # Disable some features to support TRAMP
          if [[ "$TERM" == dumb ]]; then
            unsetopt ZLE PROMPT_CR PROMPT_SUBST
            unset RPS1 RPROMPT
            PS1='$ '
            PROMPT='$ '
          fi
        '';
      initExtraFirst = optionalString shellCfg.tmux.autoRun ''
        if [[ -z "$TMUX" &&
              -z "$EMACS" &&
              -z "$VIMRUNTIME" &&
              -z "$INSIDE_EMACS" &&
              "$TERM_PROGRAM" != 'WezTerm' ]]; then
          tmux start-server

          if ! tmux has-session 2>/dev/null; then
            tmux new-session -ds main \; \
                 set-option -t main destroy-unattached off
          fi

          exec tmux attach-session -d
        fi
      '';
      envExtra = cfg.envInit;
    };

    user.packages = with pkgs; [
      zinit
      # zinit dependencies
      curl
      file
      git
      subversion

      atuin
      fzf
      starship
      zoxide
      (mkIf stdenv.isDarwin terminal-notifier)
    ];

    home.dataFile."zinit/zinit.git".source = "${pkgs.zinit}/share/zinit";

    home.configFile."atuin/config.toml".source = "${configDir}/atuin/config.toml";

    home.configFile."starship.toml".text = ''
      ${builtins.readFile "${starship-nerd-font-symbols-preset}/etc/starship.toml"}
    '';

    environment.shells = [
      "/run/current-system/sw/bin/zsh"
      "${pkgs.zsh}/bin/zsh"
    ];

    user.shell = pkgs.zsh;

    modules.shell.enable = true;
  };
}

{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  inherit (types) lines;

  shellCfg = config.modules.shell;
  cfg = shellCfg.zsh;
in
{
  options.modules.shell.zsh = {
    enable = mkEnableOption "Z Shell";

    envInit = mkOption {
      type = lines;
      default = "";
    };
    rcInit = mkOption {
      type = lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      envExtra = ''
        ${cfg.envInit}
      '';
      autosuggestion.enable = true;
      defaultKeymap = "emacs";
      initContent = ''
        bindkey '^u' backward-kill-line
        bindkey -M menuselect '^[[Z' reverse-menu-complete
        autoload -Uz select-word-style
        select-word-style bash

        # zsh-fzf-projects
        FZF_PROJECTS_WORKSPACE_DIRS=( ~/ghq )
        FZF_PROJECTS_WORKSPACE_MAX_DEPTH=3
        FZF_PROJECTS_KNOWN_PROJECTS=( /etc/dotfiles )
        zle -N fzf-projects
        bindkey '^o' fzf-projects

        # TODO: Add cd-gitroot plugin (mollifier/cd-gitroot)
        # TODO: Add zsh-wez-man plugin (sei40kr/zsh-wez-man) for WezTerm

        ${cfg.rcInit}
      '';
      localVariables = {
        KEYTIMEOUT = 1;
      };
      setOptions = [
        "AUTO_PUSHD"
        "PUSHD_IGNORE_DUPS"
        "AUTO_LIST"
        "AUTO_MENU"
        "AUTO_PARAM_KEYS"
        "AUTO_PARAM_SLASH"
        "EQUALS"
        "GLOB_DOTS"
        "NUMERIC_GLOB_SORT"
        "HIST_FCNTL_LOCK"
        "HIST_REDUCE_BLANKS"
        "INTERACTIVE_COMMENTS"
        "PRINT_EIGHT_BIT"
        "AUTO_RESUME"
        "PROMPT_SUBST"
        "NO_BEEP"
      ];
      history = {
        append = true;
        size = 10000;
        ignoreDups = true;
        ignoreAllDups = true;
        ignoreSpace = true;
        share = true;
      };
      plugins = [
        {
          name = "zsh-autopair";
          src = pkgs.zsh-autopair;
          file = "share/zsh/zsh-autopair/autopair.zsh";
        }
        {
          name = "F-Sy-H";
          src = pkgs.zsh-f-sy-h;
          file = "share/zsh/site-functions/F-Sy-H.plugin.zsh";
        }
        {
          name = "fzf-projects";
          src = inputs.zsh-fzf-projects;
        }
      ];
      oh-my-zsh = {
        enable = true;
        plugins = [
          "extract"
          "bgnotify"
          "git-auto-fetch"
          "fancy-ctrl-z"
        ];
        extraConfig = ''
          HYPHEN_INSENSITIVE=true
        '';
      };
      zsh-abbr = {
        enable = true;
        abbreviations = config.programs.zsh.shellAliases;
      };
    };

    modules.shell.enable = true;
    modules.shell.atuin.enable = true;
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    # TODO: Disable ^R and \\ec
    programs.fzf = {
      enable = true;
      defaultCommand = "${pkgs.fd}/bin/fd -HL -t f -E .git";
      defaultOptions = [
        "--cycle"
        "--reverse"
        "--info=inline"
      ];
      fileWidgetCommand = "${pkgs.fd}/bin/fd -HL -t f -E .git";
      fileWidgetOptions = [ "--preview='${pkgs.bat}/bin/bat -pp --color=always {}'" ];
      changeDirWidgetOptions = [ "--preview='${pkgs.eza}/bin/eza -la {}'" ];
    };
    programs.navi.enable = true;
    modules.shell.starship.enable = true;
    programs.zoxide.enable = true;
  };
}

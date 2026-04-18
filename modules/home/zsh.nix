{
  config,
  inputs,
  lib,
  perSystem,
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
      defaultKeymap = "emacs";
      initContent = ''
        bindkey '^u' backward-kill-line
        autoload -Uz select-word-style
        select-word-style bash

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
    };

    programs.sheldon = {
      enable = true;
      enableZshIntegration = true;
      settings = {
        shell = "zsh";
        templates.defer = ''
          {% for file in files %}
          zsh-defer source "{{ file }}"
          {% endfor %}
        '';
        plugins = {
          "00-zsh-defer" = {
            local = "${pkgs.zsh-defer}/share/zsh-defer";
          };
          "01-omz-lib-completion" = {
            local = "${pkgs.oh-my-zsh}/share/oh-my-zsh/lib";
            use = [ "completion.zsh" ];
            hooks.pre = "HYPHEN_INSENSITIVE=true";
            hooks.post = "bindkey -M menuselect '^[[Z' reverse-menu-complete";
          };
          omz-extract = {
            local = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/extract";
            use = [ "*.plugin.zsh" ];
            apply = [ "defer" ];
          };
          omz-bgnotify = {
            local = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/bgnotify";
            use = [ "*.plugin.zsh" ];
            apply = [ "defer" ];
          };
          omz-git-auto-fetch = {
            local = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/git-auto-fetch";
            use = [ "*.plugin.zsh" ];
            apply = [ "defer" ];
          };
          omz-fancy-ctrl-z = {
            local = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/fancy-ctrl-z";
            use = [ "*.plugin.zsh" ];
            apply = [ "defer" ];
          };
          zsh-autopair = {
            local = "${pkgs.zsh-autopair}/share/zsh/zsh-autopair";
            apply = [ "defer" ];
          };
          zsh-autosuggestions = {
            local = "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions";
            apply = [ "defer" ];
          };
          zsh-patina = {
            inline = ''zsh-defer eval "$(${perSystem.zsh-patina.default}/bin/zsh-patina activate)"'';
          };
          fzf-projects = {
            local = "${inputs.zsh-fzf-projects}";
            apply = [ "defer" ];
            hooks.post = ''
              FZF_PROJECTS_WORKSPACE_DIRS=( ~/ghq )
              FZF_PROJECTS_WORKSPACE_MAX_DEPTH=3
              FZF_PROJECTS_KNOWN_PROJECTS=( /etc/dotfiles )
              zle -N fzf-projects
              bindkey '^o' fzf-projects
            '';
          };
          zsh-abbr = {
            local = "${pkgs.zsh-abbr}/share/zsh/zsh-abbr";
          };
        };
      };
    };

    xdg.configFile."zsh-abbr/user-abbreviations".text =
      lib.concatStringsSep "\n" (
        lib.mapAttrsToList (k: v: "abbr ${k}=${lib.escapeShellArg v}") config.programs.zsh.shellAliases
      )
      + "\n";

    xdg.configFile."zsh-patina/config.toml".text = ''
      [highlighting]
      theme = "tokyonight"
    '';

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

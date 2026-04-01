{
  config,
  inputs,
  pkgs,
  ...
}:
{
  imports = [ inputs.self.homeModules.home-shared ];

  nix.gc = {
    automatic = true;
    dates = "daily";
  };

  nixpkgs.config.allowUnfree = true;

  programs.nh = {
    enable = true;
    flake = "${config.home.homeDirectory}/dotfiles";
  };

  modules.ai.agent-browser.enable = true;
  modules.ai.claude-code = {
    enable = true;
    claudebox.enable = true;
    ccstatusline.enable = true;
  };

  modules.desktop.aerospace.enable = true;

  modules.dev.lang.java.enable = true;
  modules.dev.lang.javascript.enable = true;
  modules.dev.lang.nix.enable = true;
  modules.dev.lang.shell.enable = true;
  modules.dev.lang.sql.enable = true;
  modules.dev.lang.web.enable = true;

  modules.dev.tools.aws.enable = true;
  modules.dev.tools.docker = {
    enable = true;
    compose.enable = true;
  };
  modules.dev.tools.github.enable = true;
  modules.dev.tools.gitu.enable = true;
  modules.dev.tools.k8s.enable = true;
  modules.dev.tools.terraform.enable = true;

  modules.editors.fonts = {
    code = {
      package = pkgs.nerd-fonts.iosevka;
      name = "Iosevka Nerd Font Mono";
      size = 18;
    };
    ui = {
      name = "sans-serif";
      size = 16;
    };
  };
  modules.editors.ideavim.enable = true;
  modules.editors.lazyvim.enable = true;

  modules.shell.apps.fastfetch.enable = true;
  modules.shell.git.enable = true;
  modules.shell.zsh.enable = true;

  modules.term.font = {
    package = pkgs.nerd-fonts.iosevka;
    name = "Iosevka Nerd Font Mono";
    size = 18;
  };
  modules.term.colorschemes.active = "tokyo-night";
  modules.term.ghostty.enable = true;
  modules.term.tmux.enable = true;

  # Add Rancher Desktop executables to PATH
  home.sessionPath = [ "${config.home.homeDirectory}/.rd/bin" ];

  home.packages = with pkgs; [
    google-cloud-sdk
    jq
  ];

  home.stateVersion = "25.11";
}

{
  inputs,
  pkgs,
  ...
}:
{
  imports = [ inputs.self.homeModules.home-shared ];

  programs.nh = {
    enable = true;
    flake = "/etc/dotfiles";
  };

  modules.ai.agent-browser.enable = true;
  modules.ai.claude-code = {
    enable = true;
    claudebox.enable = true;
    ccstatusline.enable = true;
  };

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
      name = "Iosevka NFM";
      size = 18;
    };
    ui = {
      name = "sans-serif";
      size = 16;
    };
  };
  modules.editors.idea.enable = true;
  modules.editors.ideavim.enable = true;
  modules.editors.lazyvim.enable = true;

  modules.shell.apps.fastfetch.enable = true;
  modules.shell.git.enable = true;
  modules.shell.zsh.enable = true;

  modules.term.font = {
    package = pkgs.nerd-fonts.iosevka;
    name = "Iosevka NFM";
    size = 18;
  };
  modules.term.colorschemes.active = "tokyo-night";
  modules.term.tmux.enable = true;
  modules.term.wezterm.enable = true;

  home.packages = with pkgs; [
    google-cloud-sdk
    jq
  ];

  home.stateVersion = "25.11";
}

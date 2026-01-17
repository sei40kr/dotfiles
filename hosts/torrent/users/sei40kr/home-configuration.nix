{
  inputs,
  perSystem,
  pkgs,
  ...
}:
{
  imports = [ inputs.self.homeModules.home-shared ];

  # Enable automatic garbage collection
  # NOTE: This will only garbage collect the user's profiles
  nix.gc = {
    automatic = true;
    dates = ''
      *-*-* 03:00:00
    '';
  };

  programs.nh = {
    enable = true;
    flake = "/etc/dotfiles";
  };

  modules.desktop.apps.bitwarden.enable = true;
  modules.desktop.apps.dunst.enable = true;
  modules.desktop.apps.gnome.pomodoro.enable = true;
  modules.desktop.apps.quickshell.enable = true;
  modules.desktop.apps.swww.enable = true;

  modules.desktop.media.foliate.enable = true;

  modules.dev.lang.cc.enable = true;
  modules.dev.lang.go.enable = true;
  modules.dev.lang.java.enable = true;
  modules.dev.lang.lua.enable = true;
  modules.dev.lang.r.enable = true;
  modules.dev.lang.nix.enable = true;
  modules.dev.lang.python.enable = true;
  modules.dev.lang.rust.enable = true;
  modules.dev.lang.shell.enable = true;
  modules.dev.lang.sql.enable = true;
  modules.dev.lang.web = {
    enable = true;
    bun.enable = true;
  };

  modules.dev.tools.ansible.enable = true;
  modules.dev.tools.aws.enable = true;
  modules.dev.tools.jupyter.enable = true;

  modules.editors.fonts.code = {
    package = pkgs.nerd-fonts.iosevka;
    name = "Iosevka NFM";
    size = 17;
  };
  modules.editors.emacs = {
    enable = true;
    doom.enable = true;
  };
  modules.editors.idea.enable = true;
  modules.editors.lazyvim.enable = true;

  modules.shell.apps.fastfetch.enable = true;
  modules.shell.git.enable = true;
  modules.shell.oj.enable = true;
  modules.shell.zsh.enable = true;

  modules.term.font = {
    package = perSystem.self.julia-mono-nf;
    name = "JuliaMono Nerd Font";
    size = 18;
  };
  modules.term.colorschemes.active = "tokyo-night";
  modules.term.kitty.enable = true;
  modules.term.wezterm.enable = true;

  home.packages = with pkgs; [ vlc ];

  home.stateVersion = "23.11";
}

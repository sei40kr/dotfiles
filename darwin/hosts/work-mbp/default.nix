{ darwinSystem }:

darwinSystem "aarch64-darwin" ({ pkgs, ... }:
let
  victor-mono-nf = pkgs.nerdfonts.override { fonts = [ "VictorMono" ]; };
in
rec {
  networking.hostName = "work-mbp";

  home-manager.users.${user.name}.home.stateVersion = "22.05";

  modules.desktop.apps.amphetamine.enable = true;
  modules.desktop.apps.bitwarden.enable = true;
  modules.desktop.apps.notion-calendar.enable = true;
  modules.desktop.apps.dash.enable = true;
  modules.desktop.apps.just-focus.enable = true;
  modules.desktop.apps.pocket.enable = true;
  modules.desktop.apps.raycast.enable = true;
  modules.desktop.apps.rectangle.enable = true;
  modules.desktop.apps.slack.enable = true;
  modules.desktop.apps.todoist.enable = true;
  modules.desktop.apps.zoom.enable = true;

  modules.dev.lang.javascript.enable = true;
  modules.dev.lang.nix.enable = true;
  modules.dev.lang.ruby = {
    enable = true;
    rails.enable = true;
  };
  modules.dev.lang.web = {
    enable = true;
    bun.enable = true;
  };
  modules.dev.tools.mutagen = {
    enable = true;
    compose.enable = true;
  };

  modules.editors.nvim.enable = true;
  modules.editors.idea.enable = true;
  modules.editors.datagrip.enable = true;

  modules.services.docker.enable = true;

  modules.shell.zsh.enable = true;
  modules.shell.tmux = {
    enable = true;
    autoRun = true;
  };
  modules.shell.git = {
    enable = true;
    user.email = "yongju.seong@codetakt.com";
  };
  modules.shell.ghq.enable = true;
  modules.shell.apps.fastfetch.enable = true;

  modules.term.colorschemes.active = "tokyo-night";
  modules.term.kitty.enable = true;

  modules.editors.fonts.code = {
    package = victor-mono-nf;
    name = "VictorMono Nerd Font Mono";
    size = 24;
  };
  modules.term.font = {
    package = pkgs.nerdfonts.override { fonts = [ "IosevkaTerm" ]; };
    name = "IosevkaTerm Nerd Font Mono";
    size = 28;
  };

  user.name = "sei40kr";
})

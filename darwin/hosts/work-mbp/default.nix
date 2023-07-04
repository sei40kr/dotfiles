{ lib, pkgs', ... }:

with lib;
let
  system = "aarch64-darwin";
  pkgs = pkgs'.${system};
in
{
  inherit system;

  stateVersion = "22.05";

  modules.desktop.apps.amphetamine.enable = true;
  modules.desktop.apps.bitwarden.enable = true;
  modules.desktop.apps.cron.enable = true;
  modules.desktop.apps.dash.enable = true;
  modules.desktop.apps.just-focus.enable = true;
  modules.desktop.apps.pocket.enable = true;
  modules.desktop.apps.raycast.enable = true;
  modules.desktop.apps.rectangle.enable = true;
  modules.desktop.apps.slack.enable = true;
  modules.desktop.apps.todoist.enable = true;

  modules.dev.javascript.enable = true;
  modules.dev.nix.enable = true;
  modules.dev.ruby = {
    enable = true;
    rails.enable = true;
  };
  modules.dev.web.enable = true;
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
  modules.shell.git.enable = true;
  modules.shell.ghq.enable = true;

  modules.term.theme.active = "tokyo-night";
  modules.term.font = {
    package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
    name = "Iosevka Nerd Font";
    size = 24;
  };
  modules.term.kitty.enable = true;

  user.name = "sei40kr";
}

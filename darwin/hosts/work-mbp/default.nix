{ lib, pkgs', ... }:

with lib;
let
  system = "aarch64-darwin";
  pkgs = pkgs'.${system};
in
{
  inherit system;

  stateVersion = "22.05";

  modules.desktop.apps.alfred.enable = true;
  modules.desktop.apps.rectangle.enable = true;
  modules.desktop.apps.zoom.enable = true;

  modules.dev.java.enable = true;
  modules.dev.javascript.enable = true;
  modules.dev.nix.enable = true;
  modules.dev.rust.enable = true;
  modules.dev.web.enable = true;
  modules.dev.tools.pre-commit.enable = true;

  modules.editors.nvim.enable = true;
  modules.editors.emacs = {
    enable = true;
    doom.enable = true;
  };
  modules.editors.idea.enable = true;
  modules.editors.datagrip.enable = true;

  modules.shell.zsh.enable = true;
  modules.shell.tmux = {
    enable = true;
    autoRun = true;
  };
  modules.shell.git.enable = true;
  modules.shell.ghq.enable = true;

  modules.term.theme.active = "doom-one";
  modules.term.font = {
    package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
    name = "Iosevka Nerd Font";
    size = 24;
  };
  modules.term.kitty.enable = true;

  user.name = "sei40kr";
}

{ lib, pkgs', ... }:

with lib;
let
  system = "x86_64-darwin";
  pkgs = pkgs'.${system};
in
{
  inherit system;

  modules = {
    dev = {
      aws-cli.enable = true;
      java.enable = true;
      javascript.enable = true;
      nix.enable = true;
      web.enable = true;
    };
    editors = {
      emacs = {
        enable = true;
        doom.enable = true;
      };
      ideavim.enable = true;
      nvim.enable = true;
    };
    shell = {
      zsh.enable = true;
      tmux = {
        enable = true;
        autoRun = true;
      };
      git.enable = true;
      kaggle.enable = true;
    };
  };

  modules.shell.ghq.enable = true;

  modules.term.kitty.enable = true;

  modules.term.font = {
    package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
    name = "Iosevka Nerd Font";
    size = 20;
  };

  modules.term.theme.active = "doom-one";

  user.name = "RLSUU178967";
}

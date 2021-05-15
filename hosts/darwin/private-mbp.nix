{ lib, pkgs, ... }:

with lib;
with lib.my; {
  time.timeZone = "Asia/Tokyo";

  modules = {
    desktop = {
      apps.dash.enable = true;
    };
    dev = {
      ansible.enable = true;
      aws-cli.enable = true;
      cc.enable = true;
      go.enable = true;
      google-cloud-sdk.enable = true;
      groovy.enable = true;
      haskell.enable = true;
      java.enable = true;
      javascript.enable = true;
      jupyter.enable = true;
      kotlin.enable = true;
      latex.enable = true;
      python.enable = true;
      ruby = {
        enable = true;
        rails.enable = true;
      };
      rust.enable = true;
      scala.enable = true;
      shell.enable = true;
      spring-boot.enable = true;
      sql.enable = true;
      web.enable = true;
    };
    editors = {
      emacs = {
        enable = true;
        doom.enable = true;
      };
      font = {
        family = "Input";
        size = 17;
      };
      neovim = {
        enable = true;
        manpager.enable = true;
      };
      vscode.enable = true;
    };
    shell = {
      atcoder-tools.enable = true;
      bat.enable = true;
      exa.enable = true;
      git.enable = true;
      hugo.enable = true;
      htop.enable = true;
      kaggle.enable = true;
      prettyping.enable = true;
      online-judge-tools.enable = true;
      tcpdump.enable = true;
      tmux = {
        enable = true;
        autoRun.enable = true;
      };
      zsh.enable = true;
    };

    colorschemes.active = "doom-one";
  };

  user = {
    name = "sei40kr";
    shell = pkgs.zsh;
    uid = 1000;
  };
}

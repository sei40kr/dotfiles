{ pkgs, ... }:

{
  my.userName = "rlsuu178967";
  my.userFullName = "Seong Yong-ju";
  my.userEmail = "seong_yongju@waku-2.com";
  my.user = {
    uid = 1000;
    shell = pkgs.zsh;
  };

  modules = {
    dev = {
      editors = {
        emacs.enable = true;
        neovim.enable = true;
      };
      javascript.enable = true;
      web.enable = true;
      tools.git.enable = true;
    };
    shell = {
      zsh.enable = true;
      tmux = {
        enable = true;
        autostart = true;
      };
      tools.atcoderTools.enable = true;
    };
    # term.iterm2.enable = true;
  };
}

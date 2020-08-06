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
    dev.editors.neovim.enable = true;
    shell = {
      zsh.enable = true;
      tmux = {
        enable = true;
        autostart = true;
      };
    };
    term.iterm2.enable = true;
  };
}

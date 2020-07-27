{ pkgs, ... }:

{
  my.userName = "rlsuu178967";
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
  };
}

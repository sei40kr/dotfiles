{ fetchFromGitHub, lib, pkgs, ... }:

with lib; {
  dein-vim = pkgs.vimUtils.buildVimPlugin {
    pname = "dein-vim";
    version = "2.0";
    src = fetchFromGitHub {
      owner = "Shougo";
      repo = "dein.vim";
      rev = "67755968dc3ad3f7950e682f0d35eb02c6e4372e";
      sha256 = "1qd3i7cj4lb87s4k6irnxwvws43wm58jr3psizc5wg9i4gijxkd8";
    };
    dependencies = with pkgs; [ git ];
  };
}

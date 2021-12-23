{ fetchFromGitHub, git, lib, vimUtils, ... }:

with lib;
let
in {
  dein-vim = vimUtils.buildVimPluginFrom2Nix rec {
    pname = "dein-vim";
    version = "2.2";

    src = fetchFromGitHub {
      owner = "Shougo";
      repo = "dein.vim";
      rev = version;
      sha256 = "0p4w9qb714vz11c9mk2hs3zkirmyb8c6zinci9qb4d9v0hvnrgzz";
    };

    dependencies = [ git ];
  };
}

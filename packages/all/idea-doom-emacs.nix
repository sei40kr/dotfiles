{ fetchFromGitHub, lib, vimUtils, ... }:

with lib;
vimUtils.buildVimPlugin {
  pname = "idea-doom-emacs";
  version = "unstable-2021-05-14";
  namePrefix = "";
  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "idea-doom-emacs";
    rev = "42eb397f0a80bc32925ab2f75ac2229e8d312c9b";
    sha256 = "07f3jp5gsdxhfpryrpv74svv991zj54vl5wnbw9lip3vjw0bqp2p";
  };
  path = "idea-doom-emacs";
}

{ fetchFromGitHub, lib, vimUtils, ... }:

with lib;
vimUtils.buildVimPlugin {
  pname = "idea-doom-emacs";
  version = "unstable-2021-05-13";
  namePrefix = "";
  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "idea-doom-emacs";
    rev = "4aa2eb51c121be9cbe0aa45258bd566935158fae";
    sha256 = "0rzvwliqcpp6jhhxaaqyn94f0z54l1gjxwpsp6nnv3l9rii97hv7";
  };
  path = "idea-doom-emacs";
}

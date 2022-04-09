{ fetchFromGitHub }:

fetchFromGitHub {
  owner = "vinceliuice";
  repo = "WhiteSur-cursors";
  rev = "2cb7219ba1ac6483219285a887061bfe516d813e";
  sha256 = "sha256-/eUH6EIWqzwUgHw1ZV8mws9Kip7W4t8RkInd0K22Nv4=";
  leaveDotGit = true;
  postFetch = ''
    shopt -s extglob
    mkdir -p $out/share/icons
    cp -r $out/dist $out/share/icons/WhiteSur-cursors
    rm -rf $out/{.git,!(share)}
  '';
}

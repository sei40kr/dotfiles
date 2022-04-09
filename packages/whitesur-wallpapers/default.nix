{ fetchFromGitHub }:

fetchFromGitHub {
  owner = "vinceliuice";
  repo = "WhiteSur-wallpapers";
  rev = "v1.0";
  sha256 = "sha256-8H46/ujn+v4VC80D41joo1vgiIMly7JyrGPK+bjhV/8=";
  leaveDotGit = true;
  postFetch = ''
    export HOME=$(mktemp -d)
    bash $out/install-wallpapers.sh
    rm -rf $out/{.git,*}
    mv $HOME/.local/share $out
    rm -rf $HOME
  '';
}

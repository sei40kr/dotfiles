{ pkgs }:

pkgs.fetchFromGitHub {
  owner = "vinceliuice";
  repo = "WhiteSur-wallpapers";
  rev = "v2.0";
  hash = "sha256-b2gN0sU2VppIm9ckrv9MIhJZ2CpNMFOAMI6eCtSPaFM=";

  postFetch = ''
    export HOME=$(mktemp -d)
    bash $out/install-wallpapers.sh
    rm -rf $out
    mkdir -p $out
    mv $HOME/.local/share $out/share
    rm -rf $HOME
  '';

  meta = with pkgs.lib; {
    description = "WhiteSur wallpapers";
    homepage = "https://github.com/vinceliuice/WhiteSur-wallpapers";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}

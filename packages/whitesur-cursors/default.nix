{ fetchFromGitHub, lib }:

fetchFromGitHub {
  owner = "vinceliuice";
  repo = "WhiteSur-cursors";
  rev = "2cb7219ba1ac6483219285a887061bfe516d813e";
  hash = "sha256-/eUH6EIWqzwUgHw1ZV8mws9Kip7W4t8RkInd0K22Nv4=";

  postFetch = ''
    shopt -s extglob
    mkdir -p $out/share/icons
    cp -r $out/dist $out/share/icons/WhiteSur-cursors
    rm -rf $out/!(share)
  '';

  meta = with lib; {
    description = "WhiteSur cursors theme for linux desktops";
    homepage = "https://github.com/vinceliuice/WhiteSur-cursors";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
  };
}

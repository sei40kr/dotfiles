{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "whitesur-kde";
  version = "unstable-2023-02-21";

  src = pkgs.fetchFromGitHub {
    owner = "vinceliuice";
    repo = "WhiteSur-kde";
    rev = "a1ff2c2d751d7a475dac7733dd7d4ff3071a0a7b";
    hash = "sha256-bnHpn3VQjlT78F+y5PaYxFw199g7We7B4DmCO8MngOI=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/{aurorae/themes,color-schemes,Kvantum,plasma/desktoptheme,sddm/themes,wallpapers}
    cp -r aurorae/* $out/share/aurorae/themes
    cp -r color-schemes/*.colors $out/share/color-schemes
    cp -r Kvantum/* $out/share/Kvantum
    cp -r plasma/desktoptheme/WhiteSur* $out/share/plasma/desktoptheme
    cp -r plasma/desktoptheme/icons $out/share/plasma/desktoptheme/WhiteSur
    cp -r plasma/desktoptheme/icons $out/share/plasma/desktoptheme/WhiteSur-alt
    cp -r plasma/desktoptheme/icons $out/share/plasma/desktoptheme/WhiteSur-dark
    cp -r plasma/look-and-feel $out/share/plasma/look-and-feel
    cp -r sddm/WhiteSur $out/share/sddm/themes
    cp -r wallpaper/WhiteSur $out/share/wallpapers

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "MacOS big sur theme for kde plasma";
    homepage = "https://github.com/vinceliuice/WhiteSur-kde";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
  };
}

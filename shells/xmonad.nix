{ haskellPackages, mkShell }:

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (hPkgs: [
      hPkgs.xmonad
      hPkgs.xmonad-contrib
      hPkgs.xmonad-extras
    ]))
  ];
}

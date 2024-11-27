{ haskellPackages, mkShell }:

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (hPkgs: [
      hPkgs.xmonad
      hPkgs.xmonad-contrib_0_18_1
      hPkgs.xmonad-extras
    ]))
  ];
}

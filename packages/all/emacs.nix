{ emacsPgtkGcc, fetchFromGitHub, lib, stdenv, ... }:

# Emacs 28 + native-comp + pgtk
with lib;
let
  inherit (stdenv) isDarwin;
  emacsPlus = fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    rev = "aac87685c893e3de07f77c077bcaf3403f1ceb8a";
    sha256 = "1d3znwhcafxlwp8bqzwy07mgwr868z22pfnivw88213sp10v5zsk";
  };
in ((emacsPgtkGcc.override { withXwidgets = !isDarwin; }).overrideAttrs
  ({ patches ? [ ], postInstall ? "", ... }: {
    patches = patches ++ (optionals isDarwin [
      "${emacsPlus}/patches/emacs-28/fix-window-role.patch"
      "${emacsPlus}/patches/emacs-28/no-frame-refocus-cocoa.patch"
      "${emacsPlus}/patches/emacs-28/system-appearance.patch"
    ]);

    postInstall = postInstall + (optionalString isDarwin ''
      cp -f ${emacsPlus}/icons/nobu417-big-sur.icns $out/Applications/Emacs.app/Contents/Resources/Emacs.icns
    '');
  }))

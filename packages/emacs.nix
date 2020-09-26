{ autoconf, emacs, fetchpatch, stdenv, ... }:

let emacsPlusRev = "f5aefc8ee706914eba102104c6021018340dfaf1";
in if stdenv.isDarwin then
  emacs.overrideAttrs (oldAttrs: {
    patches = [
      (fetchpatch {
        url =
          "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/${emacsPlusRev}/patches/emacs-27/fix-window-role.patch";
        sha256 = "1hcfm6dxy2ji7q8fw502757920axffy32qlk9pcmpmk6q1zclgzv";
      })
      (fetchpatch {
        url =
          "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/${emacsPlusRev}/patches/emacs-27/ligatures-freeze-fix.patch";
        sha256 = "0zldjs8nx26x7r8pwjc995lvpg06iv52rq4cy1w38hxhy7vp8lp3";
      })
      (fetchpatch {
        url =
          "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/${emacsPlusRev}/patches/emacs-27/system-appearance.patch";
        sha256 = "0cdhzrbcfpfdsk6x1dmmrrfas4hzivzhv461z4vhwqii1pxn8n7m";
      })
      (fetchpatch {
        url =
          "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/${emacsPlusRev}/patches/emacs-27/xwidgets_webkit_in_cocoa.patch";
        sha256 = "1vp08j3qvb4xl5rgfc1l5ybxfy3raj7mj4aydsgkmd6zwzkw74hg";
      })
    ];

    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ autoconf ];
  })
else
  emacs

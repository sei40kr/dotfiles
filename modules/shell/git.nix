{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.shell.git;

  gitignoreFile = pkgs.writeTextFile {
    name = "gitignore-global";
    text = ''
      # -*- mode: gitignore; -*-

      # Created by https://www.gitignore.io/api/emacs,macos,intellij
      # Edit at https://www.gitignore.io/?templates=emacs,macos,intellij

      ### Emacs ###
      *~
      \#*\#
      /.emacs.desktop
      /.emacs.desktop.lock
      *.elc
      auto-save-list
      tramp
      .\#*

      # Org-mode
      .org-id-locations
      *_archive

      # flymake-mode
      *_flymake.*

      # eshell files
      /eshell/history
      /eshell/lastdir

      # elpa packages
      /elpa/

      # reftex files
      *.rel

      # AUCTeX auto folder
      /auto/

      # cask packages
      .cask/
      dist/

      # Flycheck
      flycheck_*.el

      # server auth directory
      /server/

      # projectiles files
      .projectile

      # directory configuration
      .dir-locals.el

      ### Intellij ###
      # Covers JetBrains IDEs: IntelliJ, RubyMine, PhpStorm, AppCode, PyCharm, CLion, Android Studio and WebStorm
      # Reference: https://intellij-support.jetbrains.com/hc/en-us/articles/206544839

      # User-specific stuff
      .idea/**/workspace.xml
      .idea/**/tasks.xml
      .idea/**/usage.statistics.xml
      .idea/**/dictionaries
      .idea/**/shelf

      # Generated files
      .idea/**/contentModel.xml

      # Sensitive or high-churn files
      .idea/**/dataSources/
      .idea/**/dataSources.ids
      .idea/**/dataSources.local.xml
      .idea/**/sqlDataSources.xml
      .idea/**/dynamic.xml
      .idea/**/uiDesigner.xml
      .idea/**/dbnavigator.xml

      # Gradle
      .idea/**/gradle.xml
      .idea/**/libraries

      # Gradle and Maven with auto-import
      # When using Gradle or Maven with auto-import, you should exclude module files,
      # since they will be recreated, and may cause churn.  Uncomment if using
      # auto-import.
      # .idea/modules.xml
      # .idea/*.iml
      # .idea/modules

      # CMake
      cmake-build-*/

      # Mongo Explorer plugin
      .idea/**/mongoSettings.xml

      # File-based project format
      *.iws

      # IntelliJ
      out/

      # mpeltonen/sbt-idea plugin
      .idea_modules/

      # JIRA plugin
      atlassian-ide-plugin.xml

      # Cursive Clojure plugin
      .idea/replstate.xml

      # Crashlytics plugin (for Android Studio and IntelliJ)
      com_crashlytics_export_strings.xml
      crashlytics.properties
      crashlytics-build.properties
      fabric.properties

      # Editor-based Rest Client
      .idea/httpRequests

      # Android studio 3.1+ serialized cache file
      .idea/caches/build_file_checksums.ser

      ### Intellij Patch ###
      # Comment Reason: https://github.com/joeblau/gitignore.io/issues/186#issuecomment-215987721

      # *.iml
      # modules.xml
      # .idea/misc.xml
      # *.ipr

      # Sonarlint plugin
      .idea/sonarlint

      ### macOS ###
      # General
      .DS_Store
      .AppleDouble
      .LSOverride

      # Icon must end with two \r
      Icon

      # Thumbnails
      ._*

      # Files that might appear in the root of a volume
      .DocumentRevisions-V100
      .fseventsd
      .Spotlight-V100
      .TemporaryItems
      .Trashes
      .VolumeIcon.icns
      .com.apple.timemachine.donotpresent

      # Directories potentially created on remote AFP share
      .AppleDB
      .AppleDesktop
      Network Trash Folder
      Temporary Items
      .apdisk

      # End of https://www.gitignore.io/api/emacs,macos,intellij
    '';
  };
in
{
  options.modules.shell.git = with types; {
    enable = mkBoolOpt false;

    user = {
      name = mkOption {
        type = types.str;
        default = "Seong Yong-ju";
        description = mdDoc ''
          The name of the user.
        '';
      };

      email = mkOption {
        type = types.str;
        default = "sei40kr@gmail.com";
        description = mdDoc ''
          The email of the user.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      git
      gitAndTools.gitflow
      gitui
    ];

    programs.git = {
      enable = true;
      config = {
        branch = {
          autosetuprebase = "always";
          "master" = {
            merge = "refs/heads/master";
            remote = "origin";
          };
        };
        commit.verbose = true;
        core = {
          excludesFile = gitignoreFile;
          pager = "${pkgs.delta}/bin/delta";
          whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
        };
        diff = {
          compactionHeuristic = true;
          indentHeuristic = true;
        };
        difftool.prompt = false;
        fetch.prune = true;
        github.user = "sei40kr";
        interactive.diffFilter = "${pkgs.delta}/bin/delta --color-only";
        merge.conflictstyle = "diff3";
        rebase.autosquash = true;
        rerere.enabled = true;
        push.default = "current";
        stash.showPatch = true;
        url."ssh://git@github.com/sei40kr".insteadOf = "https://github.com/sei40kr";
        user = {
          email = cfg.user.email;
          name = cfg.user.name;
        };
      };
      lfs.enable = true;
    };

    home.configFile."gitui/key_bindings.ron".source = "${configDir}/gitui/key_bindings.ron";
  };
}

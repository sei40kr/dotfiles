# macos-user-defaults.pl --- macOS user defaults
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
## Based on @mathiasbynens's dotfiles
## https://github.com/mathiasbynens/dotfiles/blob/master/.macos

    # Always show scrollbars
    defaults_write_string( 'NSGlobalDomain', 'AppleShowScrollBars', 'Always' );

    # Disable the over-the-top focus ring animation
    defaults_write_bool( 'NSGlobalDomain', 'NSUseAnimatedFocusRing', 0 );

    # Increase window resize speed for Cocoa applications
    defaults_write_float( 'NSGlobalDomain', 'NSWindowResizeTime', 0.001 );

    # Expand save panel by default
    defaults_write_bool( 'NSGlobalDomain',
        'NSNavPanelExpandedStateForSaveMode', 1 );
    defaults_write_bool( 'NSGlobalDomain',
        'NSNavPanelExpandedStateForSaveMode2', 1 );

    # Expand print panel by default
    defaults_write_bool( 'NSGlobalDomain', 'PMPrintingExpandedStateForPrint',
        1 );
    defaults_write_bool( 'NSGlobalDomain', 'PMPrintingExpandedStateForPrint2',
        1 );

    # Save to disk (not to iCloud) by default
    defaults_write_bool( 'NSGlobalDomain', 'NSDocumentSaveNewDocumentsToCloud',
        0 );

    # Automatically quit printer app once the print jobs complete
    defaults_write_bool( 'com.apple.print.PrintingPrefs',
        'Quit When Finished', 1 );

    # Disable the “Are you sure you want to open this application?” dialog
    defaults_write_bool( 'com.apple.LaunchServices', 'LSQuarantine', 0 );

  # Display ASCII control characters using caret notation in standard text views
  # Try e.g. `cd /tmp; unidecode "\x{0000}" > cc.txt; open -e cc.txt`
    defaults_write_bool( 'NSGlobalDomain', 'NSTextShowsControlCharacters', 1 );

    # Disable Resume system-wide
    defaults_write_bool( 'com.apple.systempreferences',
        'NSQuitAlwaysKeepsWindows', 0 );

    # Disable automatic termination of inactive apps
    defaults_write_bool( 'NSGlobalDomain', 'NSDisableAutomaticTermination', 1 );

    # Disable the crash reporter
    defaults_write_string( 'com.apple.CrashReporter', 'DialogType', 'none' );

    # Disable automatic capitalization as it’s annoying when typing code
    defaults_write_bool( 'NSGlobalDomain', 'NSAutomaticCapitalizationEnabled',
        0 );

    # Disable smart dashes as they’re annoying when typing code
    defaults_write_bool( 'NSGlobalDomain',
        'NSAutomaticDashSubstitutionEnabled', 0 );

    # Disable automatic period substitution as it’s annoying when typing code
    defaults_write_bool( 'NSGlobalDomain',
        'NSAutomaticPeriodSubstitutionEnabled', 0 );

    # Disable smart quotes as they’re annoying when typing code
    defaults_write_bool( 'NSGlobalDomain',
        'NSAutomaticQuoteSubstitutionEnabled', 0 );

    # Disable auto-correct
    defaults_write_bool( 'NSGlobalDomain',
        'NSAutomaticSpellingCorrectionEnabled', 0 );

## Keyboard and input

    # Enable full keyboard access for all controls
    # (e.g. enable Tab in modal dialogs)
    defaults_write_int( 'NSGlobalDomain', 'AppleKeyboardUIMode', 3 );

    # Set a blazingly fast keyboard repeat rate
    defaults_write_int( 'NSGlobalDomain', 'KeyRepeat',        1 );
    defaults_write_int( 'NSGlobalDomain', 'InitialKeyRepeat', 10 );

## Screen

    # Save screenshots to the desktop
    defaults_write_string( 'com.apple.screencapture', 'location',
        "${ENV{HOME}}/Desktop" );

    # Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
    defaults_write_string( 'com.apple.screencapture', 'type', 'png' );

    # Disable shadow in screenshots
    defaults_write_bool( 'com.apple.screencapture', 'disable-shadow', 1 );

## Finder

    # Finder: disable window animations and Get Info animations
    defaults_write_bool( 'com.apple.finder', 'DisableAllAnimations', 1 );

    # Set Desktop as the default location for new Finder windows
    # For other paths, use `PfLo` and `file:///full/path/here/`
    defaults_write_string( 'com.apple.finder', 'NewWindowTarget', 'PfLo' );
    defaults_write_string( 'com.apple.finder', 'NewWindowTargetPath',
        "file://${ENV{HOME}}" );

    # Show icons for hard drives, servers, and removable media on the desktop
    defaults_write_bool( 'com.apple.finder', 'ShowExternalHardDrivesOnDesktop',
        1 );
    defaults_write_bool( 'com.apple.finder', 'ShowHardDrivesOnDesktop',     1 );
    defaults_write_bool( 'com.apple.finder', 'ShowMountedServersOnDesktop', 1 );
    defaults_write_bool( 'com.apple.finder', 'ShowRemovableMediaOnDesktop', 1 );

    # Finder: show hidden files by default
    #defaults write com.apple.finder AppleShowAllFiles -bool 1
    # Finder: show all filename extensions
    defaults_write_bool( 'NSGlobalDomain', 'AppleShowAllExtensions', 1 );

    # Finder: show status bar
    defaults_write_bool( 'com.apple.finder', 'ShowStatusBar', 1 );

    # Finder: show path bar
    defaults_write_bool( 'com.apple.finder', 'ShowPathbar', 1 );

    # Display full POSIX path as Finder window title
    defaults_write_bool( 'com.apple.finder', '_FXShowPosixPathInTitle', 1 );

    # Keep folders on top when sorting by name
    defaults_write_bool( 'com.apple.finder', '_FXSortFoldersFirst', 1 );

    # When performing a search, search the current folder by default
    defaults_write_string( 'com.apple.finder', 'FXDefaultSearchScope', "SCcf" );

    # Disable the warning when changing a file extension
    defaults_write_bool( 'com.apple.finder', 'FXEnableExtensionChangeWarning',
        0 );

    # Enable spring loading for directories
    defaults_write_bool( 'NSGlobalDomain', 'com.apple.springing.enabled', 1 );

    # Remove the spring loading delay for directories
    defaults_write_float( 'NSGlobalDomain', 'com.apple.springing.delay', 0 );

    # Avoid creating .DS_Store files on network or USB volumes
    defaults_write_bool( 'com.apple.desktopservices',
        'DSDontWriteNetworkStores', 1 );
    defaults_write_bool( 'com.apple.desktopservices', 'DSDontWriteUSBStores',
        1 );

    # Disable disk image verification
    defaults_write_bool( 'com.apple.frameworks.diskimages', 'skip-verify', 1 );
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'skip-verify-locked', 1 );
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'skip-verify-remote', 1 );

    # Automatically open a new Finder window when a volume is mounted
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'auto-open-ro-root', 1 );
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'auto-open-rw-root', 1 );
    defaults_write_bool( 'com.apple.finder', 'OpenWindowForNewRemovableDisk',
        1 );

    # Use list view in all Finder windows by default
    # Four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`
    defaults_write_string( 'com.apple.finder', 'FXPreferredViewStyle', 'Nlsv' );

    # Disable the warning before emptying the Trash
    defaults_write_bool( 'com.apple.finder', 'WarnOnEmptyTrash', 0 );

## Dock, Dashboard, and hot corners

    # Enable highlight hover effect for the grid view of a stack (Dock)
    defaults_write_bool( 'com.apple.dock', 'mouse-over-hilite-stack', 1 );

    # Set the icon size of Dock items to 36 pixels
    defaults_write_int( 'com.apple.dock', 'tilesize', 36 );

    # Change minimize/maximize window effect
    defaults_write_string( 'com.apple.dock', 'mineffect', "scale" );

    # Minimize windows into their application’s icon
    defaults_write_bool( 'com.apple.dock', 'minimize-to-application', 1 );

    # Enable spring loading for all Dock items
    defaults_write_bool( 'com.apple.dock',
        'enable-spring-load-actions-on-all-items', 1 );

    # Show indicator lights for open applications in the Dock
    defaults_write_bool( 'com.apple.dock', 'show-process-indicators', 1 );

    # Don’t animate opening applications from the Dock
    defaults_write_bool( 'com.apple.dock', 'launchanim', 0 );

    # Speed up Mission Control animations
    defaults_write_float( 'com.apple.dock', 'expose-animation-duration', 0.1 );

    # Don’t group windows by application in Mission Control
    # (i.e. use the old Exposé behavior instead)
    # Disable Dashboard
    defaults_write_bool( 'com.apple.dashboard', 'mcx-disabled', 1 );

    # Don’t automatically rearrange Spaces based on most recent use
    defaults_write_bool( 'com.apple.dock', 'mru-spaces', 0 );

    # Remove the auto-hiding Dock delay
    defaults_write_float( 'com.apple.dock', 'autohide-delay', 0 );

    # Remove the animation when hiding/showing the Dock
    defaults_write_float( 'com.apple.dock', 'autohide-time-modifier', 0 );

    # Automatically hide and show the Dock
    defaults_write_bool( 'com.apple.dock', 'autohide', 1 );

    # Make Dock icons of hidden applications translucent
    defaults_write_bool( 'com.apple.dock', 'showhidden', 1 );

    # Don’t show recent applications in Dock
    defaults_write_bool( 'com.apple.dock', 'show-recents', 0 );

    # Disable the Launchpad gesture (pinch with thumb and three fingers)
    defaults_write_int( 'com.apple.dock', 'showLaunchpadGestureEnabled', 0 );

    # Hot corners
    # Possible values:
    #  0: no-op
    #  2: Mission Control
    #  3: Show application windows
    #  4: Desktop
    #  5: Start screen saver
    #  6: Disable screen saver
    #  7: Dashboard
    # 10: Put display to sleep
    # 11: Launchpad
    # 12: Notification Center
    # Top left screen corner → Mission Control
    defaults_write_int( 'com.apple.dock', 'wvous-tl-corner',   0 );
    defaults_write_int( 'com.apple.dock', 'wvous-tl-modifier', 0 );

    # Top right screen corner → Desktop
    defaults_write_int( 'com.apple.dock', 'wvous-tr-corner',   0 );
    defaults_write_int( 'com.apple.dock', 'wvous-tr-modifier', 0 );

    # Bottom left screen corner → Start screen saver
    defaults_write_int( 'com.apple.dock', 'wvous-bl-corner',   0 );
    defaults_write_int( 'com.apple.dock', 'wvous-bl-modifier', 0 );

## Mail                                                                        #

    # Disable send and reply animations in Mail.app
    defaults_write_bool( 'com.apple.mail', 'DisableReplyAnimations', 1 );
    defaults_write_bool( 'com.apple.mail', 'DisableSendAnimations',  1 );

# Copy email addresses as `foo@example.com` instead of `Foo Bar <foo@example.com>` in Mail.app
    defaults_write_bool( 'com.apple.mail', 'AddressesIncludeNameOnPasteboard',
        0 );

## Time Machine

   # Prevent Time Machine from prompting to use new hard drives as backup volume
    defaults_write_bool( 'com.apple.TimeMachine',
        'DoNotOfferNewDisksForBackup', 1 );

## Activity Monitor

    # Show the main window when launching Activity Monitor
    defaults_write_bool( 'com.apple.ActivityMonitor', 'OpenMainWindow', 1 );

    # Visualize CPU usage in the Activity Monitor Dock icon
    defaults_write_int( 'com.apple.ActivityMonitor', 'IconType', 5 );

    # Show all processes in Activity Monitor
    defaults_write_int( 'com.apple.ActivityMonitor', 'ShowCategory', 0 );

    # Sort Activity Monitor results by CPU usage
    defaults_write_string( 'com.apple.ActivityMonitor', 'SortColumn',
        "CPUUsage" );
    defaults_write_int( 'com.apple.ActivityMonitor', 'SortDirection', 0 );

## TextEdit, QuickTime Player

    # Use plain text mode for new TextEdit documents
    defaults_write_int( 'com.apple.TextEdit', 'RichText', 0 );

    # Open and save files as UTF-8 in TextEdit
    defaults_write_int( 'com.apple.TextEdit', 'PlainTextEncoding',         4 );
    defaults_write_int( 'com.apple.TextEdit', 'PlainTextEncodingForWrite', 4 );

    # Auto-play videos when opened with QuickTime Player
    defaults_write_bool( 'com.apple.QuickTimePlayerX', 'MGPlayMovieOnOpen', 1 );

## Mac App Store

    # Enable the automatic update check
    defaults_write_bool( 'com.apple.SoftwareUpdate', 'AutomaticCheckEnabled',
        1 );

    # Check for software updates daily, not just once per week
    defaults_write_int( 'com.apple.SoftwareUpdate', 'ScheduleFrequency', 1 );

    # Download newly available updates in background
    defaults_write_int( 'com.apple.SoftwareUpdate', 'AutomaticDownload', 1 );

    # Install System data files & security updates
    defaults_write_int( 'com.apple.SoftwareUpdate', 'CriticalUpdateInstall',
        1 );

    # Turn on app auto-update
    defaults_write_bool( 'com.apple.commerce', 'AutoUpdate', 1 );

## Photos                                                                      #

    # Prevent Photos from opening automatically when devices are plugged in
    defaults_write_bool( 'com.apple.ImageCapture', 'disableHotPlug', 1 );
}

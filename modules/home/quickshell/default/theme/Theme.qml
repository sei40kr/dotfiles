pragma Singleton
import QtQuick

QtObject {
    // Colors
    readonly property color backgroundColor: "#24283b"
    readonly property color foregroundColor: "#c0caf5"
    readonly property color activeColor: "#82aaff"
    readonly property color inactiveColor: "#636da6"

    // Semantic colors
    readonly property color mutedColor: "#565f89"    // comment - subtle/low priority
    readonly property color infoColor: "#7aa2f7"     // blue - informational
    readonly property color successColor: "#9ece6a"  // green - success/safe
    readonly property color warningColor: "#e0af68"  // yellow - warning
    readonly property color dangerColor: "#f7768e"   // red - error/critical

    // Usage indicator
    readonly property color usageBarBackground: "#414868"

    // Notification colors
    readonly property color notificationBackground: "#1a1b26" // bg_dark
    readonly property color notificationBody: "#a9b1d6"       // fg_dark
    readonly property color notificationSeparator: "#292e42"  // bg_highlight

    // Fonts
    readonly property string fontFamily: "sans-serif"
    readonly property string iconFontFamily: "Symbols Nerd Font"
    readonly property int fontSizePoints: 11
    readonly property int smallFontSizePoints: 10
    readonly property int iconSizePixels: 18

    // Layout
    readonly property int statusBarHeight: 40
}

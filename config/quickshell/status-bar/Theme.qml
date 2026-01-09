pragma Singleton
import QtQuick

QtObject {
    // Colors
    readonly property color backgroundColor: "#24283b"
    readonly property color foregroundColor: "#c0caf5"
    readonly property color activeColor: "#82aaff"
    readonly property color inactiveColor: "#636da6"

    // Usage indicator colors
    readonly property color usageBarBackground: "#414868"
    readonly property color usageSafe: "#9ece6a"      // Green (0-60%)
    readonly property color usageWarning: "#e0af68"   // Yellow (60-85%)
    readonly property color usageDanger: "#f7768e"    // Red (85-100%)

    // Fonts
    readonly property string fontFamily: "sans-serif"
    readonly property string iconFontFamily: "Symbols Nerd Font"
    readonly property int fontSizePoints: 11
    readonly property int iconSizePixels: 18
}

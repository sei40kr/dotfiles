pragma Singleton
import QtQuick

QtObject {
    // Colors
    readonly property color backgroundColor: "#24283b"
    readonly property color foregroundColor: "#c0caf5"
    readonly property color activeColor: "#82aaff"
    readonly property color inactiveColor: "#636da6"

    // Fonts
    readonly property string fontFamily: "sans-serif"
    readonly property string iconFontFamily: "Symbols Nerd Font"
    readonly property int fontSizePoints: 11
    readonly property int iconSizePixels: 18
}

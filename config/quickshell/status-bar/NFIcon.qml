import QtQuick

Text {
    required property string name

    readonly property var iconMap: ({
            "volume-muted": "\udb81\udf5f",
            "volume-off": "\udb81\udd7f",
            "volume-low": "\udb81\udd80",
            "volume-high": "\udb81\udd7e"
        })

    color: Theme.foregroundColor
    font.pixelSize: Theme.iconSizePixels
    font.family: "Symbols Nerd Font"
    text: iconMap[name] || ""
}

import QtQuick

Text {
    required property string name

    readonly property var iconMap: ({
            "volume-muted": "\udb81\udf5f",
            "volume-off": "\udb81\udd7f",
            "volume-low": "\udb81\udd80",
            "volume-high": "\udb81\udd7e",
            "memory": "\uefc5",
            "cpu": "\udb80\udf5b",
            "disk": "\uf0a0",
            "vpn": "\udb81\udd82",
            "pause": "\udb80\udfe4",
            "tomato": "\ue003",
            "coffee": "\udb80\udd76",
            "bed": "\udb82\udca0"
        })

    color: Theme.foregroundColor
    font.pixelSize: Theme.iconSizePixels
    font.family: "Symbols Nerd Font"
    text: iconMap[name] || ""
}

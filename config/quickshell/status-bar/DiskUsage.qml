import QtQuick
import QtQuick.Layouts
import "../services"

RowLayout {
    spacing: 16

    Repeater {
        model: Disk.mountPoints

        RowLayout {
            required property var modelData

            spacing: 8

            NFIcon {
                name: "disk"
            }

            Text {
                text: modelData.mountPoint
                color: Theme.foregroundColor
                font.pointSize: Theme.fontSizePoints
            }

            UsageIndicator {
                percentage: modelData.usagePercent
            }
        }
    }
}

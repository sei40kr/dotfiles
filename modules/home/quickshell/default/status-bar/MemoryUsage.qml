import QtQuick
import QtQuick.Layouts
import "../services"

RowLayout {
    spacing: 8

    NFIcon {
        name: "memory"
    }

    UsageIndicator {
        percentage: Memory.usagePercent
    }
}

import QtQuick
import QtQuick.Layouts
import "../services"

RowLayout {
    spacing: 8

    NFIcon {
        name: "cpu"
    }

    UsageIndicator {
        percentage: Cpu.cores.length === 0 ? 0 : Array.from(Cpu.cores).reduce((sum, core) => sum + (100 - core.idle), 0) / Cpu.cores.length
    }
}

import QtQuick
import QtQuick.Layouts
import "../services"

RowLayout {
    spacing: 8

    NFIcon {
        name: "cpu"
    }

    UsageIndicator {
        percentage: {
            const validCores = Array.from(Cpu.cores).filter(core => core !== null);
            return validCores.length === 0 ? 0 : validCores.reduce((sum, core) => sum + (100 - core.idle), 0) / validCores.length;
        }
    }
}

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
            if (Cpu.cores.length === 0) return 0;

            var total = 0;
            for (var i = 0; i < Cpu.cores.length; i++) {
                total += (100 - Cpu.cores[i].idle);
            }
            return total / Cpu.cores.length;
        }
    }
}

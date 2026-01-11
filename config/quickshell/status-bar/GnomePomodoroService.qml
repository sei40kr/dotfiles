import QtQuick
import Quickshell
import Quickshell.Io

Scope {
    id: service

    enum State {
        Stopped,
        Pomodoro,
        ShortBreak,
        LongBreak
    }

    property int state: GnomePomodoroService.State.Stopped
    property bool isPaused: false
    property int stateDuration: 0
    property int elapsed: 0
    readonly property int remainingSeconds: stateDuration - elapsed

    // Convert string state to enum
    function stateFromString(stateStr) {
        switch (stateStr) {
        case 'null':
        case 'stopped':
            return GnomePomodoroService.State.Stopped;
        case 'pomodoro':
            return GnomePomodoroService.State.Pomodoro;
        case 'short-break':
            return GnomePomodoroService.State.ShortBreak;
        case 'long-break':
            return GnomePomodoroService.State.LongBreak;
        default:
            console.error(`Unknown pomodoro state: ${stateStr}`);
            return GnomePomodoroService.State.Stopped;
        }
    }

    // Parse property value from gdbus output format
    function parseValue(value, propertyName) {
        // Boolean values
        if (value === 'true')
            return true;
        if (value === 'false')
            return false;

        // Numeric values (may have decimal point)
        if (/^\d+(\.\d+)?$/.test(value)) {
            return parseInt(value);
        }

        // String values (enclosed in quotes)
        const stringMatch = value.match(/'([^']+)'/);
        if (stringMatch) {
            const str = stringMatch[1];
            // Special handling for State property
            if (propertyName === 'State') {
                return stateFromString(str);
            }
            return str;
        }

        return null;
    }

    // Extract property value from gdbus output
    function extractProperty(properties, name) {
        const pattern = new RegExp(`'${name}': <([^>]+)>`);
        const match = properties.match(pattern);
        if (!match)
            return null;

        return parseValue(match[1], name);
    }

    // Update state from properties string
    function updateStateFromProperties(properties) {
        const newState = extractProperty(properties, 'State');
        const newElapsed = extractProperty(properties, 'Elapsed');
        const newStateDuration = extractProperty(properties, 'StateDuration');
        const newIsPaused = extractProperty(properties, 'IsPaused');

        if (newState !== null)
            service.state = newState;
        if (newElapsed !== null)
            service.elapsed = newElapsed;
        if (newStateDuration !== null)
            service.stateDuration = newStateDuration;
        if (newIsPaused !== null)
            service.isPaused = newIsPaused;
    }

    // Update state from PropertiesChanged signal
    function updateStateFromChange(line) {
        const propertiesMatch = line.match(/\{([^}]+)\}/);
        if (!propertiesMatch)
            return;

        const properties = propertiesMatch[1];

        // Extract each known property
        const newState = extractProperty(properties, 'State');
        const newElapsed = extractProperty(properties, 'Elapsed');
        const newStateDuration = extractProperty(properties, 'StateDuration');
        const newIsPaused = extractProperty(properties, 'IsPaused');

        // Apply updates
        if (newState !== null)
            service.state = newState;
        if (newElapsed !== null)
            service.elapsed = newElapsed;
        if (newStateDuration !== null)
            service.stateDuration = newStateDuration;
        if (newIsPaused !== null)
            service.isPaused = newIsPaused;
    }

    // Get initial properties
    Process {
        id: initialPropertiesProc
        running: true
        command: ["gdbus", "call", "--session", "--dest", "org.gnome.Pomodoro", "--object-path", "/org/gnome/Pomodoro", "--method", "org.freedesktop.DBus.Properties.GetAll", "org.gnome.Pomodoro"]

        stdout: SplitParser {
            onRead: data => {
                service.updateStateFromProperties(data);
            }
        }
    }

    // Monitor property changes
    Process {
        id: monitorProc
        running: true
        command: ["gdbus", "monitor", "--session", "--dest", "org.gnome.Pomodoro", "--object-path", "/org/gnome/Pomodoro"]

        stdout: SplitParser {
            onRead: data => {
                if (data.includes("org.freedesktop.DBus.Properties.PropertiesChanged")) {
                    service.updateStateFromChange(data);
                }
            }
        }
    }
}

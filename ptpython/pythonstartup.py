import os
import sys
from importlib import import_module


sys.path.append(
    os.path.join(
        os.getenv("XDG_CONFIG_HOME", os.path.expanduser("~/.config")), "ptpython",
    )
)

config = import_module("config")


try:
    from ptpython.repl import embed
except ImportError:
    print("ptpython is not available: falling back to standard prompt")
else:
    sys.exit(embed(globals(), locals(), configure=config.configure))

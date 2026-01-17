# Configuration file for jupyter-console.

# ------------------------------------------------------------------------------
# ConnectionFileMixin(LoggingConfigurable) configuration
# ------------------------------------------------------------------------------
## Mixin for configurable classes that work with connection files

## JSON file in which to store connection info [default: kernel-<pid>.json]
#
#  This file will contain the IP, ports, and authentication key needed to connect
#  clients to this kernel. By default, this file will be created in the security
#  dir of the current profile, but can be specified by absolute path.
#  Default: ''
# c.ConnectionFileMixin.connection_file = ''

## set the control (ROUTER) port [default: random]
#  Default: 0
# c.ConnectionFileMixin.control_port = 0

## set the heartbeat port [default: random]
#  Default: 0
# c.ConnectionFileMixin.hb_port = 0

## set the iopub (PUB) port [default: random]
#  Default: 0
# c.ConnectionFileMixin.iopub_port = 0

## Set the kernel's IP address [default localhost]. If the IP address is
#  something other than localhost, then Consoles on other machines will be able
#  to connect to the Kernel, so be careful!
#  Default: ''
# c.ConnectionFileMixin.ip = ''

## set the shell (ROUTER) port [default: random]
#  Default: 0
# c.ConnectionFileMixin.shell_port = 0

## set the stdin (ROUTER) port [default: random]
#  Default: 0
# c.ConnectionFileMixin.stdin_port = 0

#  Choices: any of ['tcp', 'ipc'] (case-insensitive)
#  Default: 'tcp'
# c.ConnectionFileMixin.transport = 'tcp'

# ------------------------------------------------------------------------------
# JupyterConsoleApp(ConnectionFileMixin) configuration
# ------------------------------------------------------------------------------
## Set to display confirmation dialog on exit. You can always use 'exit' or
#  'quit', to force a direct exit without any confirmation.
#  Default: True
# c.JupyterConsoleApp.confirm_exit = True

## JSON file in which to store connection info [default: kernel-<pid>.json]
#  See also: ConnectionFileMixin.connection_file
# c.JupyterConsoleApp.connection_file = ''

## set the control (ROUTER) port [default: random]
#  See also: ConnectionFileMixin.control_port
# c.JupyterConsoleApp.control_port = 0

## Connect to an already running kernel
#  Default: ''
# c.JupyterConsoleApp.existing = ''

## set the heartbeat port [default: random]
#  See also: ConnectionFileMixin.hb_port
# c.JupyterConsoleApp.hb_port = 0

## set the iopub (PUB) port [default: random]
#  See also: ConnectionFileMixin.iopub_port
# c.JupyterConsoleApp.iopub_port = 0

## Set the kernel's IP address [default localhost].
#  See also: ConnectionFileMixin.ip
# c.JupyterConsoleApp.ip = ''

## The kernel manager class to use.
#  Default: 'jupyter_client.manager.KernelManager'
# c.JupyterConsoleApp.kernel_manager_class = 'jupyter_client.manager.KernelManager'

## The name of the default kernel to start.
#  Default: 'python'
# c.JupyterConsoleApp.kernel_name = 'python'

## set the shell (ROUTER) port [default: random]
#  See also: ConnectionFileMixin.shell_port
# c.JupyterConsoleApp.shell_port = 0

## Path to the ssh key to use for logging in to the ssh server.
#  Default: ''
# c.JupyterConsoleApp.sshkey = ''

## The SSH server to use to connect to the kernel.
#  Default: ''
# c.JupyterConsoleApp.sshserver = ''

## set the stdin (ROUTER) port [default: random]
#  See also: ConnectionFileMixin.stdin_port
# c.JupyterConsoleApp.stdin_port = 0

#  See also: ConnectionFileMixin.transport
# c.JupyterConsoleApp.transport = 'tcp'

# ------------------------------------------------------------------------------
# Application(SingletonConfigurable) configuration
# ------------------------------------------------------------------------------
## This is an application.

## The date format used by logging formatters for %(asctime)s
#  Default: '%Y-%m-%d %H:%M:%S'
# c.Application.log_datefmt = '%Y-%m-%d %H:%M:%S'

## The Logging format template
#  Default: '[%(name)s]%(highlevel)s %(message)s'
# c.Application.log_format = '[%(name)s]%(highlevel)s %(message)s'

## Set the log level by value or name.
#  Choices: any of [0, 10, 20, 30, 40, 50, 'DEBUG', 'INFO', 'WARN', 'ERROR', 'CRITICAL']
#  Default: 30
# c.Application.log_level = 30

## Instead of starting the Application, dump configuration to stdout
#  Default: False
# c.Application.show_config = False

## Instead of starting the Application, dump configuration to stdout (as JSON)
#  Default: False
# c.Application.show_config_json = False

# ------------------------------------------------------------------------------
# JupyterApp(Application) configuration
# ------------------------------------------------------------------------------
## Base class for Jupyter applications

## Answer yes to any prompts.
#  Default: False
# c.JupyterApp.answer_yes = False

## Full path of a config file.
#  Default: ''
# c.JupyterApp.config_file = ''

## Specify a config file to load.
#  Default: ''
# c.JupyterApp.config_file_name = ''

## Generate default config file.
#  Default: False
# c.JupyterApp.generate_config = False

## The date format used by logging formatters for %(asctime)s
#  See also: Application.log_datefmt
# c.JupyterApp.log_datefmt = '%Y-%m-%d %H:%M:%S'

## The Logging format template
#  See also: Application.log_format
# c.JupyterApp.log_format = '[%(name)s]%(highlevel)s %(message)s'

## Set the log level by value or name.
#  See also: Application.log_level
# c.JupyterApp.log_level = 30

## Instead of starting the Application, dump configuration to stdout
#  See also: Application.show_config
# c.JupyterApp.show_config = False

## Instead of starting the Application, dump configuration to stdout (as JSON)
#  See also: Application.show_config_json
# c.JupyterApp.show_config_json = False

# ------------------------------------------------------------------------------
# ZMQTerminalIPythonApp(JupyterApp, JupyterConsoleApp) configuration
# ------------------------------------------------------------------------------
## Answer yes to any prompts.
#  See also: JupyterApp.answer_yes
# c.ZMQTerminalIPythonApp.answer_yes = False

## Full path of a config file.
#  See also: JupyterApp.config_file
# c.ZMQTerminalIPythonApp.config_file = ''

## Specify a config file to load.
#  See also: JupyterApp.config_file_name
# c.ZMQTerminalIPythonApp.config_file_name = ''

##
#  See also: JupyterConsoleApp.confirm_exit
# c.ZMQTerminalIPythonApp.confirm_exit = True

## JSON file in which to store connection info [default: kernel-<pid>.json]
#  See also: ConnectionFileMixin.connection_file
# c.ZMQTerminalIPythonApp.connection_file = ''

## set the control (ROUTER) port [default: random]
#  See also: ConnectionFileMixin.control_port
# c.ZMQTerminalIPythonApp.control_port = 0

## Connect to an already running kernel
#  See also: JupyterConsoleApp.existing
# c.ZMQTerminalIPythonApp.existing = ''

## Generate default config file.
#  See also: JupyterApp.generate_config
# c.ZMQTerminalIPythonApp.generate_config = False

## set the heartbeat port [default: random]
#  See also: ConnectionFileMixin.hb_port
# c.ZMQTerminalIPythonApp.hb_port = 0

## set the iopub (PUB) port [default: random]
#  See also: ConnectionFileMixin.iopub_port
# c.ZMQTerminalIPythonApp.iopub_port = 0

## Set the kernel's IP address [default localhost].
#  See also: ConnectionFileMixin.ip
# c.ZMQTerminalIPythonApp.ip = ''

## The kernel manager class to use.
#  See also: JupyterConsoleApp.kernel_manager_class
# c.ZMQTerminalIPythonApp.kernel_manager_class = 'jupyter_client.manager.KernelManager'

## The name of the default kernel to start.
#  See also: JupyterConsoleApp.kernel_name
# c.ZMQTerminalIPythonApp.kernel_name = 'python'

## The date format used by logging formatters for %(asctime)s
#  See also: Application.log_datefmt
# c.ZMQTerminalIPythonApp.log_datefmt = '%Y-%m-%d %H:%M:%S'

## The Logging format template
#  See also: Application.log_format
# c.ZMQTerminalIPythonApp.log_format = '[%(name)s]%(highlevel)s %(message)s'

## Set the log level by value or name.
#  See also: Application.log_level
# c.ZMQTerminalIPythonApp.log_level = 30

## set the shell (ROUTER) port [default: random]
#  See also: ConnectionFileMixin.shell_port
# c.ZMQTerminalIPythonApp.shell_port = 0

## Instead of starting the Application, dump configuration to stdout
#  See also: Application.show_config
# c.ZMQTerminalIPythonApp.show_config = False

## Instead of starting the Application, dump configuration to stdout (as JSON)
#  See also: Application.show_config_json
# c.ZMQTerminalIPythonApp.show_config_json = False

## Path to the ssh key to use for logging in to the ssh server.
#  See also: JupyterConsoleApp.sshkey
# c.ZMQTerminalIPythonApp.sshkey = ''

## The SSH server to use to connect to the kernel.
#  See also: JupyterConsoleApp.sshserver
# c.ZMQTerminalIPythonApp.sshserver = ''

## set the stdin (ROUTER) port [default: random]
#  See also: ConnectionFileMixin.stdin_port
# c.ZMQTerminalIPythonApp.stdin_port = 0

#  See also: ConnectionFileMixin.transport
# c.ZMQTerminalIPythonApp.transport = 'tcp'

# ------------------------------------------------------------------------------
# ZMQTerminalInteractiveShell(SingletonConfigurable) configuration
# ------------------------------------------------------------------------------
## Text to display before the first prompt. Will be formatted with variables
#  {version} and {kernel_banner}.
#  Default: 'Jupyter console {version}\n\n{kernel_banner}'
# c.ZMQTerminalInteractiveShell.banner = 'Jupyter console {version}\n\n{kernel_banner}'

## Callable object called via 'callable' image handler with one argument, `data`,
#  which is `msg["content"]["data"]` where `msg` is the message from iopub
#  channel.  For example, you can find base64 encoded PNG data as
#  `data['image/png']`. If your function can't handle the data supplied, it
#  should return `False` to indicate this.
#  Default: None
# c.ZMQTerminalInteractiveShell.callable_image_handler = None

## Options for displaying tab completions, 'column', 'multicolumn', and
#  'readlinelike'. These options are for `prompt_toolkit`, see `prompt_toolkit`
#  documentation for more information.
#  Choices: any of ['column', 'multicolumn', 'readlinelike']
#  Default: 'multicolumn'
# c.ZMQTerminalInteractiveShell.display_completions = 'multicolumn'

## Shortcut style to use at the prompt. 'vi' or 'emacs'.
#  Default: 'emacs'
# c.ZMQTerminalInteractiveShell.editing_mode = 'emacs'

## Highlight matching brackets.
#  Default: True
# c.ZMQTerminalInteractiveShell.highlight_matching_brackets = True

## The name of a Pygments style to use for syntax highlighting
#  Default: ''
# c.ZMQTerminalInteractiveShell.highlighting_style = ''

## Override highlighting format for specific tokens
#  Default: {}
# c.ZMQTerminalInteractiveShell.highlighting_style_overrides = {}

## How many history items to load into memory
#  Default: 1000
# c.ZMQTerminalInteractiveShell.history_load_length = 1000

## Handler for image type output.  This is useful, for example, when connecting
#  to the kernel in which pylab inline backend is activated.  There are four
#  handlers defined.  'PIL': Use Python Imaging Library to popup image; 'stream':
#  Use an external program to show the image.  Image will be fed into the STDIN
#  of the program.  You will need to configure `stream_image_handler`;
#  'tempfile': Use an external program to show the image.  Image will be saved in
#  a temporally file and the program is called with the temporally file.  You
#  will need to configure `tempfile_image_handler`; 'callable': You can set any
#  Python callable which is called with the image data.  You will need to
#  configure `callable_image_handler`.
#  Choices: any of ['PIL', 'stream', 'tempfile', 'callable'] or None
#  Default: 'PIL'
# c.ZMQTerminalInteractiveShell.image_handler = 'PIL'

## Whether to include output from clients other than this one sharing the same
#  kernel.
#  Default: False
# c.ZMQTerminalInteractiveShell.include_other_output = False

## Timeout (in seconds) for giving up on a kernel's is_complete response.
#
#  If the kernel does not respond at any point within this time, the kernel will
#  no longer be asked if code is complete, and the console will default to the
#  built-in is_complete test.
#  Default: 1
# c.ZMQTerminalInteractiveShell.kernel_is_complete_timeout = 1

## Timeout for giving up on a kernel (in seconds).
#
#  On first connect and restart, the console tests whether the kernel is running
#  and responsive by sending kernel_info_requests. This sets the timeout in
#  seconds for how long the kernel can take before being presumed dead.
#  Default: 60
# c.ZMQTerminalInteractiveShell.kernel_timeout = 60

## Preferred object representation MIME type in order.  First matched MIME type
#  will be used.
#  Default: ['image/png', 'image/jpeg', 'image/svg+xml']
# c.ZMQTerminalInteractiveShell.mime_preference = ['image/png', 'image/jpeg', 'image/svg+xml']

## Prefix to add to outputs coming from clients other than this one.
#
#  Only relevant if include_other_output is True.
#  Default: 'Remote '
# c.ZMQTerminalInteractiveShell.other_output_prefix = 'Remote '

## Display the current vi mode (when using vi editing mode).
#  Default: True
# c.ZMQTerminalInteractiveShell.prompt_includes_vi_mode = True

## Use simple fallback prompt. Features may be limited.
#  Default: False
# c.ZMQTerminalInteractiveShell.simple_prompt = False

## Command to invoke an image viewer program when you are using 'stream' image
#  handler.  This option is a list of string where the first element is the
#  command itself and reminders are the options for the command.  Raw image data
#  is given as STDIN to the program.
#  Default: []
# c.ZMQTerminalInteractiveShell.stream_image_handler = []

## Command to invoke an image viewer program when you are using 'tempfile' image
#  handler.  This option is a list of string where the first element is the
#  command itself and reminders are the options for the command.  You can use
#  {file} and {format} in the string to represent the location of the generated
#  image file and image format.
#  Default: []
# c.ZMQTerminalInteractiveShell.tempfile_image_handler = []

## Use 24bit colors instead of 256 colors in prompt highlighting. If your
#  terminal supports true color, the following command should print 'TRUECOLOR'
#  in orange: printf "\x1b[38;2;255;100;0mTRUECOLOR\x1b[0m\n"
#  Default: False
# c.ZMQTerminalInteractiveShell.true_color = False

## Whether to use the kernel's is_complete message handling. If False, then the
#  frontend will use its own is_complete handler.
#  Default: True
# c.ZMQTerminalInteractiveShell.use_kernel_is_complete = True

# ------------------------------------------------------------------------------
# KernelManager(ConnectionFileMixin) configuration
# ------------------------------------------------------------------------------
## Manages a single kernel in a subprocess on this host.
#
#  This version starts kernels with Popen.

## Should we autorestart the kernel if it dies.
#  Default: True
# c.KernelManager.autorestart = True

## JSON file in which to store connection info [default: kernel-<pid>.json]
#  See also: ConnectionFileMixin.connection_file
# c.KernelManager.connection_file = ''

## set the control (ROUTER) port [default: random]
#  See also: ConnectionFileMixin.control_port
# c.KernelManager.control_port = 0

## set the heartbeat port [default: random]
#  See also: ConnectionFileMixin.hb_port
# c.KernelManager.hb_port = 0

## set the iopub (PUB) port [default: random]
#  See also: ConnectionFileMixin.iopub_port
# c.KernelManager.iopub_port = 0

## Set the kernel's IP address [default localhost].
#  See also: ConnectionFileMixin.ip
# c.KernelManager.ip = ''

## DEPRECATED: Use kernel_name instead.
#
#  The Popen Command to launch the kernel. Override this if you have a custom
#  kernel. If kernel_cmd is specified in a configuration file, Jupyter does not
#  pass any arguments to the kernel, because it cannot make any assumptions about
#  the arguments that the kernel understands. In particular, this means that the
#  kernel does not receive the option --debug if it given on the Jupyter command
#  line.
#  Default: []
# c.KernelManager.kernel_cmd = []

## set the shell (ROUTER) port [default: random]
#  See also: ConnectionFileMixin.shell_port
# c.KernelManager.shell_port = 0

## Time to wait for a kernel to terminate before killing it, in seconds. When a
#  shutdown request is initiated, the kernel will be immediately send and
#  interrupt (SIGINT), followedby a shutdown_request message, after 1/2 of
#  `shutdown_wait_time`it will be sent a terminate (SIGTERM) request, and finally
#  at the end of `shutdown_wait_time` will be killed (SIGKILL). terminate and
#  kill may be equivalent on windows.
#  Default: 5.0
# c.KernelManager.shutdown_wait_time = 5.0

## set the stdin (ROUTER) port [default: random]
#  See also: ConnectionFileMixin.stdin_port
# c.KernelManager.stdin_port = 0

#  See also: ConnectionFileMixin.transport
# c.KernelManager.transport = 'tcp'

# ------------------------------------------------------------------------------
# KernelRestarter(LoggingConfigurable) configuration
# ------------------------------------------------------------------------------
## Monitor and autorestart a kernel.

## Whether to include every poll event in debugging output.
#
#  Has to be set explicitly, because there will be *a lot* of output.
#  Default: False
# c.KernelRestarter.debug = False

## Whether to choose new random ports when restarting before the kernel is alive.
#  Default: True
# c.KernelRestarter.random_ports_until_alive = True

## The number of consecutive autorestarts before the kernel is presumed dead.
#  Default: 5
# c.KernelRestarter.restart_limit = 5

## Kernel heartbeat interval in seconds.
#  Default: 3.0
# c.KernelRestarter.time_to_dead = 3.0

# ------------------------------------------------------------------------------
# Session(Configurable) configuration
# ------------------------------------------------------------------------------
## Object for handling serialization and sending of messages.
#
#  The Session object handles building messages and sending them with ZMQ sockets
#  or ZMQStream objects.  Objects can communicate with each other over the
#  network via Session objects, and only need to work with the dict-based IPython
#  message spec. The Session will handle serialization/deserialization, security,
#  and metadata.
#
#  Sessions support configurable serialization via packer/unpacker traits, and
#  signing with HMAC digests via the key/keyfile traits.
#
#  Parameters ----------
#
#  debug : bool
#      whether to trigger extra debugging statements
#  packer/unpacker : str : 'json', 'pickle' or import_string
#      importstrings for methods to serialize message parts.  If just
#      'json' or 'pickle', predefined JSON and pickle packers will be used.
#      Otherwise, the entire importstring must be used.
#
#      The functions must accept at least valid JSON input, and output *bytes*.
#
#      For example, to use msgpack:
#      packer = 'msgpack.packb', unpacker='msgpack.unpackb'
#  pack/unpack : callables
#      You can also set the pack/unpack callables for serialization directly.
#  session : bytes
#      the ID of this Session object.  The default is to generate a new UUID.
#  username : unicode
#      username added to message headers.  The default is to ask the OS.
#  key : bytes
#      The key used to initialize an HMAC signature.  If unset, messages
#      will not be signed or checked.
#  keyfile : filepath
#      The file containing a key.  If this is set, `key` will be initialized
#      to the contents of the file.

## Threshold (in bytes) beyond which an object's buffer should be extracted to
#  avoid pickling.
#  Default: 1024
# c.Session.buffer_threshold = 1024

## Whether to check PID to protect against calls after fork.
#
#  This check can be disabled if fork-safety is handled elsewhere.
#  Default: True
# c.Session.check_pid = True

## Threshold (in bytes) beyond which a buffer should be sent without copying.
#  Default: 65536
# c.Session.copy_threshold = 65536

## Debug output in the Session
#  Default: False
# c.Session.debug = False

## The maximum number of digests to remember.
#
#  The digest history will be culled when it exceeds this value.
#  Default: 65536
# c.Session.digest_history_size = 65536

## The maximum number of items for a container to be introspected for custom
#  serialization. Containers larger than this are pickled outright.
#  Default: 64
# c.Session.item_threshold = 64

## execution key, for signing messages.
#  Default: b''
# c.Session.key = b''

## path to file containing execution key.
#  Default: ''
# c.Session.keyfile = ''

## Metadata dictionary, which serves as the default top-level metadata dict for
#  each message.
#  Default: {}
# c.Session.metadata = {}

## The name of the packer for serializing messages. Should be one of 'json',
#  'pickle', or an import name for a custom callable serializer.
#  Default: 'json'
# c.Session.packer = 'json'

## The UUID identifying this session.
#  Default: ''
# c.Session.session = ''

## The digest scheme used to construct the message signatures. Must have the form
#  'hmac-HASH'.
#  Default: 'hmac-sha256'
# c.Session.signature_scheme = 'hmac-sha256'

## The name of the unpacker for unserializing messages. Only used with custom
#  functions for `packer`.
#  Default: 'json'
# c.Session.unpacker = 'json'

## Username for the Session. Default is your system username.
#  Default: 'sei40kr'
# c.Session.username = 'sei40kr'

c.InteractiveShellApp.exec_lines = [
    "import numpy as np\n" "import pandas as pd\n" "import mathplotlib.pyplot as plt\n"
]

local settings = require "settings"
settings.window.home_page = "www.google.co.jp"

require "downloads"
downloads.default_dir = os.getenv("HOME") .. "/Downloads"

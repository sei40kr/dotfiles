all: .zshrc.zwc zplugin.zsh.zwc 20_keymap.zsh.zwc 30_aliases.zsh.zwc 50_options.zsh.zwc 70_misc.zsh.zwc 80_custom.zsh.zwc

%.zwc: %
	zsh -fc "zcompile $*"

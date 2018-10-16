# spacemacs.pl --- Spacemacs installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_tap('d12frosted/emacs-plus');
    brew_install( 'emacs-plus', 'with-pdumper', 'HEAD' );
}
elsif (&is_arch) {
    pacman_sync('emacs');
}

git_clone( 'ssh://git@github.com/sei40kr/spacemacs.git',
    "${ENV{HOME}}/.emacs.d", 'develop' );

ln( 'spacemacs', "${ENV{HOME}}/.spacemacs.d" );

# Ansible
pip2_install('ansible-lint');

# C/C++
pip3_install('cmakelint');
pip3_install('cpplint');

# Docker
yarn_global_add('dockerfile-language-server-nodejs');

# Go
go_get('github.com/cweill/gotests/...');
go_get('github.com/davidrjenni/reftools/cmd/fillstruct');
go_get('github.com/fatih/gomodifytags');
go_get('github.com/godoctor/godoctor');
go_get('github.com/haya14busa/gopkgs/cmd/gopkgs');
go_get('github.com/josharian/impl');
go_get('github.com/nsf/gocode');
go_get('github.com/rogpeppe/godef');
go_get('github.com/zmb3/gogetdoc');
go_get('golang.org/x/tools/cmd/godoc');
go_get('golang.org/x/tools/cmd/goimports');
go_get('golang.org/x/tools/cmd/gorename');
go_get('golang.org/x/tools/cmd/guru');

# Haskell
stack_install('apply-refact');

# stack_install('ghc-mod');
stack_install('hasktags');
stack_install('hlint');
stack_install('hoogle');
stack_install('stylish-haskell');

# HTML
yarn_global_add('stylelint-cli');

# JavaScript
yarn_global_add('eslint-cli');
yarn_global_add('eslint_d');
yarn_global_add('import-js');
yarn_global_add('prettier');
yarn_global_add('prettier-eslint-cli');
yarn_global_add('tern');

# Jupyter Notebook
pip3_install('jupyter');
pip3_install('matplotlib');
if (&is_linux) {
    ln( 'matplotlib/matplotlibrc',
        "${ENV{XDG_CONFIG_HOME}}/matplotlib/matplotlibrc" );
}
elsif (&is_macos) {
    ln( 'matplotlib/matplotlibrc', "${ENV{HOME}}/.matplotlib/matplotlibrc" );
}

# Markdown
gem_install('mdl');
yarn_global_add('markdownlint-cli');
yarn_global_add('vmd');

# Perl5
cpanm('Perl::Critic');
cpanm('Perl::Tidy');

# TOOD Install plsense
# ln( 'plsense/plsense', "${ENV{HOME}}/.plsense" );

# Python
pip3_install('autopep8');
pip3_install('flake8');
pip3_install('importmagic');
pip3_install('jedi');
pip3_install('pydoc_utils');
pip3_install('pyflakes');
pip3_install('pylint');
pip3_install('pyls-isort');
pip3_install('pyls-mypy');
pip3_install('python-language-server');
pip3_install('rope');
pip3_install('yapf');

# Ruby
gem_install('fastri');
gem_install('pry');
gem_install('pry-coolline');
gem_install('pry-doc');
gem_install('rake');
gem_install('rcodetools');
gem_install('rubocop');

# Rust
cargo_install('rustfmt');

# Shell-script
if (&is_macos) {
    brew_install('shfmt');
}
elsif (&is_arch) {
    trizen_sync('shfmt-bin');
}

# SQL
go_get('github.com/jackc/sqlfmt');

# TypeScript
yarn_global_add('typescript-language-server');

# Xclipboard
if (&is_arch) {
    pacman_sync('xsel');
}

# YAML
pip3_install('yamllint');

1;

# alias_def_ruby.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# rbenv
alias rubies 'rbenv versions'
alias gemsets 'rbenv gemset list'

# ruby
alias rb ruby
alias rfind "find . -name '*.rb' | xargs grep -n"

# gem
alias gin 'gem install'
alias gun 'gem uninstall'
alias gli 'gem list'
alias gemb 'gem build \*.buildspec'
alias gemp 'gem build \*.gem'

# bundler
alias be 'bundle exec'
alias bl 'bundle list'
alias bp 'bundle package'
alias bo 'bundle open'
alias bout 'bundle outdated'
alias bu 'bundle update'
alias bi 'bundle_install'
alias bcn 'bundle clean'

# rake
alias rdm 'rake db:migrate'
alias rdms 'rake db:migrate:status'
alias rdr 'rake db:rollback'
alias rdc 'rake db:create'
alias rds 'rake db:seed'
alias rdd 'rake db:drop'
alias rdrs 'rake db:reset'
alias rdtc 'rake db:test:clone'
alias rdtp 'rake db:test:prepare'
alias rdmtc 'rake db:migrate db:test:clone'
alias rdsl 'rake db:schema:load'
alias rlc 'rake log:clear'
alias rn 'rake notes'
alias rr 'rake routes'
alias rrg 'rake routes | grep'
alias rt 'rake test'
alias rmd 'rake middleware'
alias rsts 'rake stats'

# rails
alias devlog 'tail -f log/development.log'
alias prodlog 'tail -f log/production.log'
alias testlog 'tail -f log/test.log'
alias rc 'rails console'
alias rcs 'rails console --sandbox'
alias rd 'rails destroy'
alias rdb 'rails dbconsole'
# alias rg 'rails generate'
alias rgm 'rails generate migration'
alias rp 'rails plugin'
alias ru 'rails runner'
alias rs 'rails server'
alias rsd 'rails server --debugger'
alias rsp 'rails server --port'

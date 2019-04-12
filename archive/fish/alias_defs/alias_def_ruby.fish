# alias_def_ruby.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# rbenv
alias -s rubies 'rbenv versions'
alias -s gemsets 'rbenv gemset list'

# ruby
alias -s rb ruby
alias -s rfind "find . -name '*.rb' | xargs grep -n"

# gem
alias -s gin 'gem install'
alias -s gun 'gem uninstall'
alias -s gli 'gem list'
alias -s gemb 'gem build \*.buildspec'
alias -s gemp 'gem build \*.gem'

# bundler
alias -s be 'bundle exec'
alias -s bl 'bundle list'
alias -s bp 'bundle package'
alias -s bo 'bundle open'
alias -s bout 'bundle outdated'
alias -s bu 'bundle update'
alias -s bi 'bundle_install'
alias -s bcn 'bundle clean'

# rake
alias -s rdm 'rake db:migrate'
alias -s rdms 'rake db:migrate:status'
alias -s rdr 'rake db:rollback'
alias -s rdc 'rake db:create'
alias -s rds 'rake db:seed'
alias -s rdd 'rake db:drop'
alias -s rdrs 'rake db:reset'
alias -s rdtc 'rake db:test:clone'
alias -s rdtp 'rake db:test:prepare'
alias -s rdmtc 'rake db:migrate db:test:clone'
alias -s rdsl 'rake db:schema:load'
alias -s rlc 'rake log:clear'
alias -s rn 'rake notes'
alias -s rr 'rake routes'
alias -s rrg 'rake routes | grep'
alias -s rt 'rake test'
alias -s rmd 'rake middleware'
alias -s rsts 'rake stats'

# rails
alias -s devlog 'tail -f log/development.log'
alias -s prodlog 'tail -f log/production.log'
alias -s testlog 'tail -f log/test.log'
alias -s rc 'rails console'
alias -s rcs 'rails console --sandbox'
alias -s rd 'rails destroy'
alias -s rdb 'rails dbconsole'
# alias -s rg 'rails generate'
alias -s rgm 'rails generate migration'
alias -s rp 'rails plugin'
alias -s ru 'rails runner'
alias -s rs 'rails server'
alias -s rsd 'rails server --debugger'
alias -s rsp 'rails server --port'

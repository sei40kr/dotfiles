# balias_def_ruby.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# rbenv
balias rubies 'rbenv versions'
balias gemsets 'rbenv gemset list'

# ruby
balias rb ruby
balias rfind "find . -name '*.rb' | xargs grep -n"

# gem
balias gin 'gem install'
balias gun 'gem uninstall'
balias gli 'gem list'
balias gemb 'gem build \*.buildspec'
balias gemp 'gem build \*.gem'

# bundler
balias be 'bundle exec'
balias bl 'bundle list'
balias bp 'bundle package'
balias bo 'bundle open'
balias bout 'bundle outdated'
balias bu 'bundle update'
balias bi 'bundle_install'
balias bcn 'bundle clean'

# rake
balias rdm 'rake db:migrate'
balias rdms 'rake db:migrate:status'
balias rdr 'rake db:rollback'
balias rdc 'rake db:create'
balias rds 'rake db:seed'
balias rdd 'rake db:drop'
balias rdrs 'rake db:reset'
balias rdtc 'rake db:test:clone'
balias rdtp 'rake db:test:prepare'
balias rdmtc 'rake db:migrate db:test:clone'
balias rdsl 'rake db:schema:load'
balias rlc 'rake log:clear'
balias rn 'rake notes'
balias rr 'rake routes'
balias rrg 'rake routes | grep'
balias rt 'rake test'
balias rmd 'rake middleware'
balias rsts 'rake stats'

# rails
balias devlog 'tail -f log/development.log'
balias prodlog 'tail -f log/production.log'
balias testlog 'tail -f log/test.log'
balias rc 'rails console'
balias rcs 'rails console --sandbox'
balias rd 'rails destroy'
balias rdb 'rails dbconsole'
# balias rg 'rails generate'
balias rgm 'rails generate migration'
balias rp 'rails plugin'
balias ru 'rails runner'
balias rs 'rails server'
balias rsd 'rails server --debugger'
balias rsp 'rails server --port'

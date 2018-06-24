# ruby.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# ruby

abbr -a rb ruby
abbr -a rfind "find . -name '*.rb' | xargs grep -n"

abbr -a gin gem install
abbr -a gun gem uninstall
abbr -a gli gem list

abbr -a rbb bundle
abbr -a rbbc bundle clean
abbr -a rbbe bundle exec
abbr -a rbbi bundle install --path vendor/bundle
abbr -a rbbl bundle list
abbr -a rbbo bundle open
abbr -a rbbp bundle package
abbr -a rbbu bundle update


# gem

abbr -a gemb gem build \*.buildspec
abbr -a gemp gem build \*.gem


# rake

abbr -a brake bundle exec rake


# rails

abbr -a ror bundle exec rails
abbr -a rorc bundle exec rails console
abbr -a rordc bundle exec rails dbconsole
abbr -a rordm bundle exec rake db:migrate
abbr -a rordM bundle exec rake db:migrate db:test:clone
abbr -a rordr bundle exec rake db:rollback
abbr -a rorg bundle exec rails generate
abbr -a rorl tail -f \(ruby-app-root\)/log/development.log
abbr -a rorlc bundle exec rake log:clear
abbr -a rorp bundle exec rails plugin
abbr -a rorr bundle exec rails runner
abbr -a rors bundle exec rails server
abbr -a rorsd bundle exec rails server --debugger
abbr -a rorx bundle exec rails destroy

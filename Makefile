# Makefile
# Useful tasks to install dotfiles
#
# author: Seong Yong-ju ( @sei40kr )

.PHONY: install
install:
	@ansible-playbook -c local -i localhost, --ask-sudo-pass provision/playbook.yml

.PHONY: install-test
install-test:
	@ansible-playbook -c local -i localhost, --ask-sudo-pass --check provision/playbook.yml

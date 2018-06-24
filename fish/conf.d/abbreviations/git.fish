# git.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# git

abbr -a g git

abbr -a gb git branch
abbr -a gba git branch --all --verbose
abbr -a gbc git checkout -b
abbr -a gbd git branch --delete
abbr -a gbD git branch --delete --force
abbr -a gbl git branch --verbose
abbr -a gbL git branch --all --verbose
abbr -a gbm git branch --move
abbr -a gbM git branch --move --force
abbr -a gbr git branch --move
abbr -a gbR git branch --move --force
abbr -a gbs git show-branch
abbr -a gbS git show-branch --all
abbr -a gbv git branch --verbose
abbr -a gbV git branch --verbose --verbose
abbr -a gbx git branch --delete
abbr -a gbX git branch --delete --force

abbr -a gc git commit --verbose
abbr -a gca git commit --verbose --all
abbr -a gcm git commit --message
abbr -a gcS git commit -S --verbose
abbr -a gcSa git commit -S --verbose --all
abbr -a gcSm git commit -S --message
abbr -a gcam git commit --all --message
abbr -a gco git checkout
abbr -a gcO git checkout --patch
abbr -a gcf git commit --amend --reuse-message HEAD
abbr -a gcSf git commit -S --amend --reuse-message HEAD
abbr -a gcF git commit --verbose --amend
abbr -a gcSF git commit -S --verbose --amend
abbr -a gcp git cherry-pick --ff
abbr -a gcP git cherry-pick --no-commit
abbr -a gcr git revert
abbr -a gcR git reset HEAD^
abbr -a gcs git show
abbr -a gcl git-commit-lost
abbr -a gcy git cherry -v --abbrev
abbr -a gcY git cherry -v

abbr -a gCl git --no-pager diff --name-only --diff-filter=U
abbr -a gCa git add \(git --no-pager diff --name-only --diff-filter=U\)
abbr -a gCe git mergetool \(git --no-pager diff --name-only --diff-filter=U\)
abbr -a gCo git checkout --ours --
abbr -a gCO git checkout --ours -- \(git --no-pager diff --name-only --diff-filter=U\)
abbr -a gCt git checkout --theirs --
abbr -a gCT git checkout --theirs -- \(git --no-pager diff --name-only --diff-filter=U\)

abbr -a gd git ls-files
abbr -a gdc git ls-files --cached
abbr -a gdx git ls-files --deleted
abbr -a gdm git ls-files --modified
abbr -a gdu git ls-files --other --exclude-standard
abbr -a gdk git ls-files --killed
abbr -a gdi git status --porcelain --short --ignored \| sed -n 's/^!! //p'

abbr -a gf git fetch
abbr -a gfa git fetch --all
abbr -a gfc git clone
abbr -a gfcr git clone --recurse-submodules
abbr -a gfm git pull
abbr -a gfr git pull --rebase

abbr -a gg git grep
abbr -a ggi git grep --ignore-case
abbr -a ggl git grep --files-with-matches
abbr -a ggL git grep --files-without-matches
abbr -a ggv git grep --invert-match
abbr -a ggw git grep --word-regexp

abbr -a gia git add
abbr -a giA git add --patch
abbr -a giu git add --update
abbr -a gid git diff --no-ext-diff --cached
abbr -a giD git diff --no-ext-diff --cached --word-diff
abbr -a gii git update-index --assume-unchanged
abbr -a giI git update-index --no-assume-unchanged
abbr -a gir git reset
abbr -a giR git reset --patch
abbr -a gix git rm -r --cached
abbr -a giX git rm -rf --cached

abbr -a glc git shortlog --summary --numbered

abbr -a gm git merge
abbr -a gmC git merge --no-commit
abbr -a gmF git merge --no-ff
abbr -a gma git merge --abort
abbr -a gmt git mergetool

abbr -a gp git push
abbr -a gpf git push --force-with-lease
abbr -a gpF git push --force
abbr -a gpa git push --all
abbr -a gpA git push --all\; and git push --tags
abbr -a gpt git push --tags
abbr -a gpc git push --set-upstream origin \(git-branch-current \^/dev/null\)
abbr -a gpp git pull origin \(git-branch-current \^/dev/null\)\; and git push origin \(git-branch-current \^/dev/null\)

abbr -a gr git rebase
abbr -a gra git rebase --abort
abbr -a grc git rebase --continue
abbr -a gri git rebase --interactive
abbr -a grs git rebase --skip

abbr -a gR git remote
abbr -a gRl git remote --verbose
abbr -a gRa git remote add
abbr -a gRx git remote rm
abbr -a gRm git remote rename
abbr -a gRu git remote update
abbr -a gRp git remote prune
abbr -a gRs git remote show
abbr -a gRb git-hub-browse

abbr -a gs git stash
abbr -a gsa git stash apply
abbr -a gsx git stash drop
abbr -a gsX git-stash-clear-interactive
abbr -a gsl git stash list
abbr -a gsL git-stash-dropped
abbr -a gsd git stash show --patch --stat
abbr -a gsp git stash pop
abbr -a gsr git-stash-recover
abbr -a gss git stash save --include-untracked
abbr -a gsS git stash save --patch --no-keep-index
abbr -a gsw git stash save --include-untracked --keep-index

abbr -a gS git submodule
abbr -a gSa git submodule add
abbr -a gSf git submodule foreach
abbr -a gSi git submodule init
abbr -a gSI git submodule update --init --recursive
abbr -a gSl git submodule status
abbr -a gSm git-submodule-move
abbr -a gSs git submodule sync
abbr -a gSu git submodule foreach git pull origin master
abbr -a gSx git-submodule-remove

abbr -a gt git tag
abbr -a gtl git tag -l

abbr -a gws git status --ignore-submodules=all --short
abbr -a gwS git status --ignore-submodules=all
abbr -a gwd git diff --no-ext-diff
abbr -a gwD git diff --no-ext-diff --word-diff
abbr -a gwr git reset --soft
abbr -a gwR git reset --hard
abbr -a gwc git clean -n
abbr -a gwC git clean -f
abbr -a gwx git rm -r
abbr -a gwX git rm -rf


# git-flow

abbr -a gFi git flow init
abbr -a gFf git flow feature
abbr -a gFb git flow bugfix
abbr -a gFl git flow release
abbr -a gFh git flow hotfix
abbr -a gFs git flow support
abbr -a gFfl git flow feature list
abbr -a gFfs git flow feature start
abbr -a gFff git flow feature finish
abbr -a gFfp git flow feature publish
abbr -a gFft git flow feature track
abbr -a gFfd git flow feature diff
abbr -a gFfr git flow feature rebase
abbr -a gFfc git flow feature checkout
abbr -a gFfm git flow feature pull
abbr -a gFfx git flow feature delete
abbr -a gFbl git flow bugfix list
abbr -a gFbs git flow bugfix start
abbr -a gFbf git flow bugfix finish
abbr -a gFbp git flow bugfix publish
abbr -a gFbt git flow bugfix track
abbr -a gFbd git flow bugfix diff
abbr -a gFbr git flow bugfix rebase
abbr -a gFbc git flow bugfix checkout
abbr -a gFbm git flow bugfix pull
abbr -a gFbx git flow bugfix delete
abbr -a gFll git flow release list
abbr -a gFls git flow release start
abbr -a gFlf git flow release finish
abbr -a gFlp git flow release publish
abbr -a gFlt git flow release track
abbr -a gFld git flow release diff
abbr -a gFlr git flow release rebase
abbr -a gFlc git flow release checkout
abbr -a gFlm git flow release pull
abbr -a gFlx git flow release delete
abbr -a gFhl git flow hotfix list
abbr -a gFhs git flow hotfix start
abbr -a gFhf git flow hotfix finish
abbr -a gFhp git flow hotfix publish
abbr -a gFht git flow hotfix track
abbr -a gFhd git flow hotfix diff
abbr -a gFhr git flow hotfix rebase
abbr -a gFhc git flow hotfix checkout
abbr -a gFhm git flow hotfix pull
abbr -a gFhx git flow hotfix delete
abbr -a gFsl git flow support list
abbr -a gFss git flow support start
abbr -a gFsf git flow support finish
abbr -a gFsp git flow support publish
abbr -a gFst git flow support track
abbr -a gFsd git flow support diff
abbr -a gFsr git flow support rebase
abbr -a gFsc git flow support checkout
abbr -a gFsm git flow support pull
abbr -a gFsx git flow support delete

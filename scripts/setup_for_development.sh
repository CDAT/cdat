#!/usr/bin/env bash

cd "${BASH_SOURCE%/*}/.." &&
scripts/git_setup/setup-user && echo &&
scripts/git_setup/setup-hooks && echo &&
scripts/git_setup/setup_aliases.sh && echo &&
scripts/git_setup/tips

# Rebase master by default
git config branch.master.rebase true

# Configure remote push URL.
if url="$(git config --get remote.origin.url)" &&
   echo "$url" | egrep -q '^(https?|git)://github.com/UV-CDAT/uvcdat(\.git)?$' &&
   ! pushurl="$(git config --get remote.origin.pushurl)"; then
  pushurl='git@github.com:UV-CDAT/uvcdat.git'
  echo 'Setting origin pushurl to '"$pushurl"
  git config remote.origin.pushurl "$pushurl"
fi

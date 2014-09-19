#!/usr/bin/env bash

echo "Setting up useful Git aliases..." &&

# General aliases that could be global
git config alias.prepush 'log --graph --stat origin/master..' &&

true

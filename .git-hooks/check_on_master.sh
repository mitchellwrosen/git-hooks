#!/bin/bash

curbranch=$(git rev-parse --abbrev-ref HEAD)

if [ "$curbranch" == "master" ]; then
   echo "You cannot commit to master! Stash your"
   echo "changes and apply them to another branch."
   echo
   echo "git stash"
   echo "git checkout -b new_branch_name"
   echo "git stash apply"
   echo
   exit 1
fi

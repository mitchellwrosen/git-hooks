#!/bin/bash

# Redirect output to stderr.
exec 1>&2

# Cross platform projects tend to avoid non-ascii filenames; prevent
# them from being added to the repository. We exploit the fact that the
# printable range starts at the space character and ends with tilde.

# Note that the use of brackets around a tr range is ok here, (it's
# even required, for portability to Solaris 10's /usr/bin/tr), since
# the square bracket bytes happen to fall in the designated range.
if [ $(git diff --cached --name-only --diff-filter=A -z |
     LC_ALL=C tr -d '[ -~]\0' | wc -c) != 0 ]
then
   echo " *"
   echo " * Error: Attempt to add a non-ascii file name."
   echo " *"
   echo " * This can cause problems if you want to work with"
   echo " * people on other platforms. Please rename the file."
   echo " *"
   exit 1
fi

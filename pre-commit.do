GHC_FLAGS="-v0 -Wall -Werror"

redo-ifchange pre-commit.hs

ghc $GHC_FLAGS -o $3 pre-commit.hs

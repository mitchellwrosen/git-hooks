GHC_FLAGS="-v0 -Wall -Werror -hidir obj -odir obj"

redo-ifchange pre-commit.hs System/Extras.hs

ghc $GHC_FLAGS -o $3 pre-commit.hs

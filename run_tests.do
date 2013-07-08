GHC_FLAGS="-v0 -Wall -Werror -hidir obj -odir obj"

redo-ifchange run_tests.hs

ghc $GHC_FLAGS -o $3 run_tests.hs

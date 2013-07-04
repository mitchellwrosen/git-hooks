redo-ifchange pre-commit.hs
ghc -v0 -o $3 pre-commit.hs
cp pre-commit .git/hooks

sudo apt-get update &&
yes | sudo apt-get install git &&
yes | sudo apt-get install unzip &&
yes | sudo apt-get install curl &&
yes | sudo apt-get install llvm &&
yes | sudo apt-get install clang &&
yes | sudo apt-get install haskell-platform &&
yes | sudo apt-get install haskell-platform-prof &&
cabal update &&
cabal install vector &&
cabal install hashtables &&
cabal install conduit &&
cabal install data-binary-ieee754 &&
cabal install json &&
cabal install monadlib &&
cabal install enumerator

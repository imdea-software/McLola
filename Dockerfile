FROM haskell:8.10.4-buster

# Specific libraries for MCLola
RUN cabal update && \
    cabal install --lib compdata && \
    cabal install --lib language-c99-simple && \
    cabal install --lib language-c99 && \
    cabal install --lib mtl


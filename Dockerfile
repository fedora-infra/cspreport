FROM haskell:latest

COPY . /builddir
WORKDIR /builddir

RUN apt-get update && apt-get install -y libpq-dev

RUN cabal update && cabal install --only-dependencies && cabal install

EXPOSE 8080

ENTRYPOINT cspreport

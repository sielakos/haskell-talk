FROM haskell:latest

RUN mkdir /haskell-talk
WORKDIR /haskell-talk
COPY ./ ./

FROM alpine:latest

RUN apk update \
 && apk upgrade \
 && apk add \
      binutils-gold curl gcc gmp-dev libc-dev libffi-dev make musl-dev \
      ncurses-dev perl tar xz \
 && mkdir -p /root/.ghcup/bin \
 && curl --fail --output /root/.ghcup/bin/ghcup \
      'https://downloads.haskell.org/ghcup/x86_64-linux-ghcup' \
 && chmod 0755 /root/.ghcup/bin/ghcup \
 && /root/.ghcup/bin/ghcup upgrade \
 && /root/.ghcup/bin/ghcup install ghc 9.2.4 --set

ENV PATH="/root/.ghcup/bin:$PATH"

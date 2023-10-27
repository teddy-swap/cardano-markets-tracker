FROM ubuntu:22.04 AS base
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get upgrade -y && apt-get install librocksdb-dev git liblzma-dev libnuma-dev curl automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf libncurses-dev clang llvm-13 llvm-13-dev librdkafka-dev -y

FROM base as haskell
# GHCUP
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
RUN bash -c "curl -sSL https://get.haskellstack.org/ | sh"

# Add ghcup to PATH
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin
ENV PATH=${PATH}:/usr/local
ENV PATH=${PATH}:/usr/local/bin

# install GHC and cabal
ARG GHC=8.10.7
ARG CABAL=3.6.2.0

RUN ghcup -v install ghc ${GHC} && \
    ghcup -v install cabal ${CABAL}
RUN ghcup set ghc $GHC
RUN ghcup set cabal $CABAL

FROM haskell AS cardano-markets-tracker

# # Cardano haskell dependencies
RUN git clone https://github.com/input-output-hk/libsodium
RUN cd libsodium && git checkout 66f017f1 && ./autogen.sh && ./configure && make && make install
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

# # libsecp256k1
RUN git clone https://github.com/bitcoin-core/secp256k1
RUN cd secp256k1 && git checkout ac83be33 && ./autogen.sh && ./configure --enable-module-schnorrsig --enable-experimental && make && make check && make install

ENV PATH=/usr/lib/llvm-13/bin:$PATH
RUN export CPLUS_INCLUDE_PATH=$(llvm-config --includedir):$CPLUS_INCLUDE_PATH
RUN export LD_LIBRARY_PATH=$(llvm-config --libdir):$LD_LIBRARY_PATH

WORKDIR /root/src
RUN git clone https://github.com/teddy-swap/cardano-markets-tracker.git
WORKDIR /root/src/cardano-markets-tracker
RUN git fetch --all --recurse-submodules --tags
# Cache some dependencies
RUN cabal update
# Cache the build
RUN cabal build all
ARG git_commit_id='294c088bb2617261d62d1792432c15fcb63322fb'
# ARG git_commit_id
RUN git checkout ${git_commit_id}
RUN cabal update
RUN cabal build all
RUN mkdir -p ~/.local/bin
RUN find ./dist-newstyle -name tracker-app -exec cp -p {} ~/.local/bin/ \;

FROM ubuntu:22.04 AS cardano-markets-tracker-slim
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get upgrade -y && apt-get install librocksdb-dev git liblzma-dev libnuma-dev curl automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf libncurses-dev clang llvm-13 llvm-13-dev librdkafka-dev gettext -y
RUN apt clean -y && \
    rm -rf /var/lib/apt/lists/*
RUN echo 'export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"' >> ~/.bashrc
RUN echo 'export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"' >> ~/.bashrc
COPY --from=cardano-markets-tracker /usr/local/lib/ /usr/local/lib/
COPY --from=cardano-markets-tracker /usr/local/lib/pkgconfig/ /usr/local/lib/pkgconfig/
RUN ln -s /usr/local/lib/libsodium.so.23.3.0 /usr/lib/libsodium.so.23
RUN ldconfig

COPY --from=cardano-markets-tracker /root/.local/bin/tracker-app /usr/local/bin/tracker-app
COPY ./config.dhall.template ./config.dhall.template
COPY ./generate-config.sh ./generate-config.sh
COPY ./entrypoint.sh /entrypoint.sh
COPY ./scripts /scripts
COPY ./config/cardano /config/cardano

ENV CONFIG_PATH="./config.dhall.template"
ENTRYPOINT ["/usr/local/bin/tracker-app", CONFIG_PATH]

FROM haskell:7.8

# Install database dependencies
RUN ["apt-get", "update"]
RUN ["apt-get", "-y", "install", "libpq-dev", "postgresql-client"]

# Update cabal and install yesod
RUN cabal update
ADD ./stackage /root/.cabal/stackage
RUN cat /root/.cabal/config /root/.cabal/stackage > /root/.cabal/config2
RUN ["mv", "/root/.cabal/config2", "/root/.cabal/config"]
RUN cabal update
RUN ["cabal", "install", "yesod-bin", "-j4"]

# Add our Cabal file before the rest of the codebase, so the next step can cache:
ADD ./inventorium.cabal /opt/server/inventorium.cabal

# Install Haskell dependencies.
RUN cd /opt/server && cabal install --only-dependencies -j4

# Add and build application code
#
# Note that this occurs *after* building dependencies, so when we change and rebuild application
# code, Docker can pull the previous state of the container from a cache and we will not have to
# rebuild all dependencies.
#
ADD ./ /opt/server
RUN cd /opt/server && cabal install

# Add install cabal executables to PATH
ENV PATH /root/.cabal/bin:$PATH

# Default directory for container
WORKDIR /opt/server


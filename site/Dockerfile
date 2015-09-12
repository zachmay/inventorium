FROM haskell:7.8

# Install OS-level dependencies
RUN ["apt-get", "update"]
RUN ["apt-get", "-y", "install", "wget", "sudo"]
RUN ["apt-get", "-y", "install", "libpq-dev", "postgresql-client"]

# Install Stackage
RUN wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/debian/fpco.key | sudo apt-key add -
RUN echo 'deb http://download.fpcomplete.com/debian/jessie stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
RUN sudo apt-get update && sudo apt-get install stack -y

# Add our Cabal file before the rest of the codebase, so the next step can cache:
ADD ./inventorium.cabal /opt/server/inventorium.cabal

RUN cabal update

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


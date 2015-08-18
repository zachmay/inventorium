On Mac OS X, install `boot2docker`, `docker`, and `docker-compose`:

```
brew update && brew install boot2docker docker docker-compose
```

Initialize and start the `boot2docker` VM and initialize environment variables:

```
boot2docker init
boot2docker up
eval "$(boot2docker shellinit)"
```

Now we can use `docker` to create a container for the web service on the `boot2docker` VM. This may take some time,
since it has to download the base Haskell image, run Linux package updates, and download and compile Haskell
dependencies.

```
cd site
docker build -t zachmay/yesod ./
```

Now a container image configured as described in the Dockerfile has been created. We can connect to it:

```
sudo docker run -p 3000:3000 -itv /home
```

Yesod, the Haskell web application framework we are using, can "scaffold" our site, to lay some initial groundwork.

```
docker run -p 3000:3000 -itv /Users/zmay/Projects/inventorium/site/:/opt/server zachmay/yesod /bin/bash
```





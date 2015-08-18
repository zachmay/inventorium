FROM ubuntu:14.04

# Get the nginx package and set it up
RUN ["apt-get", "update"]
RUN ["apt-get", "-y", "install", "nginx"]

# Forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log
RUN ln -sf /dev/stderr /var/log/nginx/error.log
VOLUME ["/var/cache/nginx"]
EXPOSE 80 443

# Load nginx conf as root
ADD ./site.conf /etc/nginx/sites-available/inventorium
RUN ["ln", "-s", "/etc/nginx/sites-available/inventorium", "/etc/nginx/sites-enabled/inventorium"]
RUN ["rm", "-rf", "/etc/nginx/sites-available/default"]

# Add static files as unprivileged user
RUN ["groupadd", "webserver"]
RUN ["useradd", "webserver", "-s", "/bin/bash", "-m", "-g", "webserver", "-G", "webserver"]
ENV HOME /home/webserver
WORKDIR /home/webserver
RUN ["chown", "-R", "webserver:webserver", "/home/webserver"]

# Start the server
CMD ["nginx", "-g", "daemon off;"]

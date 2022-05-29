#!/usr/bin/env bash
./stop.sh
./gwsocket --addr=127.0.0.1 -p 8080 --pipein=./send --pipeout=./recv &
export NGINX_ROOT="$(pwd)/www/"
#sudo /usr/local/nginx/sbin/nginx -c $(pwd)/nginx.conf
sudo /usr/local/openresty/bin/openresty -c $(pwd)/nginx.conf
chibi-scheme -r sudukoWs.scm &
google-chrome-stable 127.0.0.1 &
disown


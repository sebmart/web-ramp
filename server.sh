#!/bin/sh
# test lift web app

case "$1" in
'start')
    ./sbt container:start shell
    ;;
'stop')
    ./sbt container:stop
    ;;
*)
    echo "Usage: $0 { start | stop }"
    ;;
esac
exit 0
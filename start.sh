#!/bin/sh
cd `dirname $0`
# create a random vm name so multiple loggers could be started
exec erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -name ec_logger$$"@127.0.0.1" \
    -setcookie rs -s ec_logger_app start -config ebin/ec_logger

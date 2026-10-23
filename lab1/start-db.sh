#!/bin/sh
# Starts the MySQL 8.4 server for this lab.
# Native password auth is required by the mysql-haskell driver.
exec /opt/homebrew/opt/mysql@8.4/bin/mysqld_safe \
  --datadir=/opt/homebrew/var/mysql84 \
  --socket=/tmp/mysql.sock \
  --port=3306 \
  --mysql-native-password=ON

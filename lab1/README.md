# Lab 1, variant 4 — "Information services of the faculty network"

An information system written in Haskell using **type classes and instances**,
with **MySQL** as the database (7 tables).

## Structure

- `schema.sql` — database schema (7 tables) and seed data
- `src/Entity.hs` — the `Entity` and `Pretty` type classes + generic CRUD
  operations (`selectAll`, `selectWhere`, `insertEntity`, `updateEntity`,
  `deleteEntity`) that work for any instance of `Entity`
- `src/Models.hs` — one data type per table (`Kind`, `Author`, `Service`,
  `ServiceAuthor`, `User`, `Registration`, `UsageStat`) with their
  `Entity`/`Pretty` instances
- `src/DB.hs` — MySQL connection settings
- `src/Menu.hs` — interactive console menu (list / add / edit / delete /
  search services, manage authors and users, register users on services,
  record usage, usage statistics)
- `app/Main.hs` — entry point

## Requirements

- GHC + cabal (`brew install ghc cabal-install`)
- MySQL 8.4 LTS (`brew install mysql@8.4`) — the pure-Haskell driver
  `mysql-haskell` authenticates with `mysql_native_password`, which was
  removed in MySQL 9+, so the 8.4 LTS server is required.

## Setup

Initialize a separate data directory once (the main `/opt/homebrew/var/mysql`
may belong to a newer MySQL):

```sh
/opt/homebrew/opt/mysql@8.4/bin/mysqld --initialize-insecure \
  --datadir=/opt/homebrew/var/mysql84
```

Start the server (native password plugin enabled):

```sh
./start-db.sh &
```

Create the application user and the schema:

```sh
/opt/homebrew/opt/mysql@8.4/bin/mysql -u root <<'SQL'
CREATE USER IF NOT EXISTS 'lab1'@'localhost'
  IDENTIFIED WITH mysql_native_password BY 'lab1pass';
CREATE USER IF NOT EXISTS 'lab1'@'127.0.0.1'
  IDENTIFIED WITH mysql_native_password BY 'lab1pass';
GRANT ALL PRIVILEGES ON faculty_services.* TO 'lab1'@'localhost';
GRANT ALL PRIVILEGES ON faculty_services.* TO 'lab1'@'127.0.0.1';
SQL
/opt/homebrew/opt/mysql@8.4/bin/mysql -u root < schema.sql
```

## Build and run

```sh
cabal build
cabal run lab1
```

Connection settings can be overridden with the environment variables
`LAB1_DB_HOST`, `LAB1_DB_USER`, `LAB1_DB_PASS`, `LAB1_DB_NAME`
(defaults: `127.0.0.1` / `lab1` / `lab1pass` / `faculty_services`).

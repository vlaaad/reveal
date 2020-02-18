#!/usr/bin/env bash
set -e
if [ -z "$1" ]
then
    echo "No version supplied"
    exit 1
fi
clj -A:build -m version "$1"
clj -Spom
git commit -am "Release $1"
git tag "$1"
git push
git push origin "$1"
clj -A:depstar "$1.jar"
printf "Clojars Username: "
read -r username
stty -echo
printf "Clojars Password: "
read -r password
printf "\n"
stty echo
CLOJARS_USERNAME=${username} CLOJARS_PASSWORD=${password} clj -A:deploy "$1.jar"
rm "$1.jar"
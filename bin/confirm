#!/usr/bin/env bash
echo "About to execute $1 command"
echo -n "Would you like to proceed y/n? "
read reply

if [ "$reply" = y -o "$reply" = Y ]
then
   $1 "${@:2}"
else
   echo "$1 ${@:2} cancelled"
fi

#!/usr/bin/env bash

if [ "$1" = '-search' ] ; then
  if [ "$#" -eq 2 ] ; then
    swipl -s index.prolog -g "time(symds($2)),halt.";
  elif [ "$#" -eq 4 ] ; then
    swipl -s index.prolog -g "time(symds($2,$3,$4)),halt.";
  fi
fi

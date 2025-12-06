#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 <day> [input]"
  exit 1
fi

DAY=$1
INPUT=$2

if [ -z "$INPUT" ]; then
  sbt "runMain aoc.Day$DAY"
else
  sbt "runMain aoc.Day$DAY $INPUT"
fi

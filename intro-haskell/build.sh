#!/usr/bin/env bash

echo `which pandoc`
if ! [ -x `which pandoc` ]
then
  echo "Install pandoc; cabal install pandoc"
  exit
fi

echo "Build talk.pdf"
pandoc -t beamer -V theme:Warsaw --template=my.beamer slides.md -o talk.pdf
echo "talk.pdf available"

#!/bin/bash
bnfc -m Latte.cf
sed -r -i 's#(\tghc --make )TestLatte.hs( -o )TestLatte#\1Main.hs\2latte#g' Makefile
make

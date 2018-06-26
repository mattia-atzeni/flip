#!/bin/bash

rm -f source
ln -s $1 source
cd ..
make run
make clean
	
	


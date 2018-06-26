#!/bin/bash

if [ $# -gt 0 ]; then
	cd test
	rm -f source
	ln -s $1 source
	cd ..
fi

make run
make clean
	
	


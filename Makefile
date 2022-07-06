mprpr:
	cd prpr && make

m9cc:
	cd 9cc && make

mprpr1: m9cc mprpr
	# 9ccでprprをビルドする
	./prtest1.sh

clean:
	cd 9cc && make clean && cd ../prpr && make clean

.PHONY: make-prpr make-9cc

all: harp.png harp.svg
	

harp.png: harp.dot
	dot -Tpng harp.dot > harp.png

harp.svg: harp.dot
	dot -Tsvg harp.dot > harp.svg

clean:
	rm harp.png harp.svg

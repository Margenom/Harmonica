all: harmonica.png harmonica.svg
	

harmonica.png: harmonica.dot
	dot -Tpng harmonica.dot > harmonica.png

harmonica.svg: harmonica.dot
	dot -Tsvg harmonica.dot > harmonica.svg

clean:
	rm harmonica.png harmonica.svg

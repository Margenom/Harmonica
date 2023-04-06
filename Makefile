all: harmonica.svg
	

harmonica.svg: harmonica.dot
	dot -Tsvg harmonica.dot > harmonica.svg

clean:
	rm harmonica.svg

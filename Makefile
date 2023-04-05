all: harmonica.png harmonica.svg
	

harmonica.pdf: harmonica.dot
	dot -Tpdf harmonica.dot > harmonica.pdf

harmonica.svg: harmonica.dot
	dot -Tsvg harmonica.dot > harmonica.svg

clean:
	rm harmonica.png harmonica.svg

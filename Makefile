all: harmonica.pdf
	

png: harmonica.png
	

harmonica.png: harmonica.svg
	rsvg-convert harmonica.svg -f png -o harmonica.png

harmonica.pdf: harmonica.svg
	rsvg-convert harmonica.svg -f pdf -o harmonica.pdf

harmonica.svg: main.dot
	dot -Tsvg main.dot > harmonica.svg

clean:
	rm harmonica.svg
	rm harmonica.pdf harmonica.png

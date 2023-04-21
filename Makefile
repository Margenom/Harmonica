all: pdf png
	

png: harmonica.png
	

pdf: harmonica.pdf
	

harmonica.png: harmonica.svg
	inkscape --export-type=png harmonica.svg 
	#rsvg-convert harmonica.svg -f png -o harmonica.png

harmonica.pdf: harmonica.svg
	inkscape --export-type=pdf harmonica.svg 
	#rsvg-convert harmonica.svg -f pdf -o harmonica.pdf

harmonica.svg: main.dot
	dot -Tsvg main.dot > harmonica.svg

clean:
	rm harmonica.svg
	rm harmonica.pdf
	rm harmonica.png

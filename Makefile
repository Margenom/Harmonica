all: pdf png gif


gif: harmonica.gif


png: harmonica.png


pdf: harmonica.pdf


# gif - имея всего 256 цветов занимает куда меньше места чем png
harmonica.gif: harmonica.png
	magick harmonica.png harmonica.gif

harmonica.png: harmonica.svg
	inkscape --export-type=png harmonica.svg
#rsvg-convert harmonica.svg -f png -o harmonica.png # в тексте пропадает отступ

harmonica.pdf: harmonica.svg
	inkscape --export-type=pdf harmonica.svg 
#rsvg-convert harmonica.svg -f pdf -o harmonica.pdf

harmonica.svg: main.dot
	dot -Tsvg main.dot > harmonica.svg # в этом проекте использовался dot из GraphViz

writeMETA: harmonica.pdf
	exiftool harmonica.pdf -Source="https://github.com/Margenom/Harmonica" -Author="Margenom" -License="CC-BY-SA-4.0 license"

clean:
	rm harmonica.*

EislerSpec.pdf:EislerSpec.html
	wkhtmltopdf EislerSpec.html EislerSpec.pdf

EislerSpec.html: EislerSpec.md
	pandoc -s EislerSpec.md -t html5 -c github.css -o EislerSpec.html

clean:
		$(RM) *.pdf
		$(RM) *.html

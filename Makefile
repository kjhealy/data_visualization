pdfs:
	Rscript -e "suppressMessages(library(knitr));suppressMessages(library(tidyverse)); kjhslides::kjh_decktape_all_slides()"

code: .FORCE
	Rscript -e "suppressMessages(library(knitr));suppressMessages(library(tidyverse)); kjhslides::kjh_purl_all_slides()"
	find ./code -name '*.R' -type f | xargs gsed -i '1,20d'

slides: .FORCE
	Rscript -e "suppressMessages(library(knitr));suppressMessages(library(tidyverse)); kjhslides::kjh_render_all_slides()"

clean:
	find slides -type d -name '*_files' -prune -print -exec rm -rf {} +
	find slides -type d -name 'libs' -prune -print -exec rm -rf {} +
	find slides -type f -name '*.html' -prune -print -exec rm -f {} +
	find code -type f -name '*.R' -prune -print -exec rm -f {} +
	find pdf_slides -type f -name '*.pdf' -prune -print -exec rm -f {} +

.PHONY:	clean

.FORCE:
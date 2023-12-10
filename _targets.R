library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(tidyverse))

# Deployment flag—one of two tests to deploy_site target.
# See deploy_site below for the user condition

Sys.setenv(DEPLOY_VSD = FALSE)


## Parallelize things --- when we build the PDFs
## it'll take forever otherwise
library(crew)
tar_option_set(
 controller = crew_controller_local(workers = 15)
)


library(kjhslides)
library(ggthemes)
library(usethis)

# We need to return the path to the rendered HTML file. In this case,
# rmarkdown::render() *does* return a path, but it returns an absolute path,
# which makes the targets pipline less portable. So we return our own path to
# the HTML file instead.
render_quarto <- function(slide_path) {
  quarto::quarto_render(slide_path, quiet = FALSE)
  #return(paste0(tools::file_path_sans_ext(slide_path), ".html"))
}


## Use decktape (via kjhslides) to convert xaringan HTML slides to PDF.
## Return a relative path to the PDF to keep targets happy.

html_to_pdf <- function(slide_path) {
  path_sans_ext <- tools::file_path_sans_ext(slide_path)
  outdir_path <- fs::path_real(dirname(slide_path))
  kjhslides::kjh_decktape_one_slide(infile = slide_path,
                                    outdir = outdir_path)
  return(paste0(tools::file_path_sans_ext(slide_path), ".pdf"))
}

# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}


## Variables and options
page_suffix <- ".html"

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

tar_option_set(
  packages = c("tibble"),
  format = "rds",
  workspace_on_error = TRUE
)

## SITE PIPELINE ----
list(

  ## Build site ----
  tar_quarto(site, path = ".", quiet = FALSE),

  ## Convert HTML slides to PDF ----
  ### Render the built html slides in _site/slides to PDFs
  ### We wait till quarto has built the site to do this.

  tar_files(rendered_slides, {
            # Force dependencies
            site
            fl <- list.files(here_rel("slides"),
                       pattern = "\\.qmd", full.names = TRUE)
            paste0("_site/", stringr::str_replace(fl, "qmd", "html"))
            }),

  tar_target(quarto_pdfs, {
    html_to_pdf(rendered_slides)
    },
    pattern = map(rendered_slides),
    format = "file"),

  ## Upload site ----
  tar_target(deploy_script, here_rel("deploy.sh"), format = "file"),
  tar_target(deploy_site, {
    # Force dependencies
    quarto_pdfs
    # Run the deploy script if both conditions are met
    if (Sys.info()["user"] != "kjhealy" | Sys.getenv("DEPLOY_VSD") != "TRUE") message("Deployment vars not set. Will not deploy site.")
    if (Sys.info()["user"] == "kjhealy" & Sys.getenv("DEPLOY_VSD") == "TRUE") message("Running deployment script ...")
    if (Sys.info()["user"] == "kjhealy" & Sys.getenv("DEPLOY_VSD") == "TRUE") processx::run(paste0("./", deploy_script), echo = TRUE)
  })
)



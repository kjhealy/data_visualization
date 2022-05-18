

## ----codefig-first-plot, message=FALSE, fig.show="hide", fig.width=5.8, fig.height=5.5----
library(tidyverse)
library(gapminder)

p <- ggplot(data = gapminder, 
            mapping = aes(x = gdpPercap, 
                          y = lifeExp))  


p + geom_point()


## ----01-looking-at-data-1, echo=FALSE-----------------------------------------
knitr::include_graphics(
  knitr::fig_chunk("codefig-first-plot", "png")
)


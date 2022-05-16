## ----note, include=FALSE---------------------------------------------------------------------------------------------------------------------------
## NB: By default the  template will create a new subdirectory with its files inside.


## ----packages, include=FALSE-----------------------------------------------------------------------------------------------------------------------
library(flipbookr)
library(here)
library(tidyverse)
library(kjhslides)
library(socviz)
library(gapminder)


## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------
## Configure the slides

kjh_register_tenso()    # Default fonts. Comment out if you don't have Tenso and Berkeley fonts.
kjh_set_knitr_opts()    
# kjh_set_slide_theme()   # ggplot theme to go with slides. Set tenso = FALSE if necessary.
kjh_set_classic_theme()
kjh_set_xaringan_opts()



## ----03-make-a-graph-1-----------------------------------------------------------------------------------------------------------------------------
library(palmerpenguins)
penguins %>% 
  group_by(species, island, year) %>% 
  summarize(bill = round(mean(bill_length_mm, na.rm = TRUE),2)) %>% 
  knitr::kable()


## ----03-make-a-graph-2-----------------------------------------------------------------------------------------------------------------------------
penguins %>% 
  group_by(species, island, year) %>% 
  summarize(bill = round(mean(bill_length_mm, na.rm = TRUE), 2)) %>% 
  pivot_wider(names_from = year, values_from = bill) %>% 
  knitr::kable()


## ----03-make-a-graph-3-----------------------------------------------------------------------------------------------------------------------------
edu


## ----03-make-a-graph-4-----------------------------------------------------------------------------------------------------------------------------
edu %>% 
  pivot_longer(elem4:coll4, names_to = "education")


## ----03-make-a-graph-5-----------------------------------------------------------------------------------------------------------------------------
edu %>% 
  pivot_longer(elem4:coll4, names_to = "education", values_to = "n")


## ----03-make-a-graph-6-----------------------------------------------------------------------------------------------------------------------------
here() # this path will be different for you


## ----03-make-a-graph-7, echo = FALSE---------------------------------------------------------------------------------------------------------------
fs::dir_tree(here(), recurse = 0)


## ----03-make-a-graph-8-----------------------------------------------------------------------------------------------------------------------------
## Load the file relative to the path from the top of the project, without separators, etc
organs <- read_csv(file = here("data", "organdonation.csv"))


## ----03-make-a-graph-9-----------------------------------------------------------------------------------------------------------------------------
organs


## ----03-make-a-graph-10----------------------------------------------------------------------------------------------------------------------------
organs <- read_csv(file = here("data", "organdonation.csv"))


## ----03-make-a-graph-11----------------------------------------------------------------------------------------------------------------------------
organ_remote <- read_csv("http://kjhealy.co/organdonation.csv")

organ_remote


## ----03-make-a-graph-12----------------------------------------------------------------------------------------------------------------------------
gapminder


## ----03-make-a-graph-13----------------------------------------------------------------------------------------------------------------------------
dim(gapminder)


## ----03-make-a-graph-14----------------------------------------------------------------------------------------------------------------------------
p <- ggplot(data = gapminder)


## ----03-make-a-graph-15----------------------------------------------------------------------------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))


## ----03-make-a-graph-16, fig.cap='This empty plot has no geoms.', fig.width=8, fig.height=5--------------------------------------------------------
p


## ----03-make-a-graph-17, fig.cap='A scatterplot of Life Expectancy vs GDP', fig.width=8, fig.height=5----------------------------------------------
p + geom_point() 


## ----03-make-a-graph-18, fig.cap='A scatterplot of Life Expectancy vs GDP', fig.width=8, fig.height=5----------------------------------------------
p + geom_smooth() 


## ----03-make-a-graph-19, fig.cap='Life Expectancy vs GDP, using a smoother.', fig.width=8, fig.height=5--------------------------------------------

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_smooth()



## ----03-make-a-graph-20, fig.cap='Life Expectancy vs GDP, using a smoother.', fig.width=8, fig.height=5--------------------------------------------

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_point() + geom_smooth()



## ----reveal-additive1, include = FALSE-------------------------------------------------------------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_smooth() + 
  geom_point() 


## ----codefig-functionargs, message=FALSE, fig.show="hide", fig.cap="An ill-advised linear fit", fig.width=4.8, fig.height=4.5----------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_point() + 
  geom_smooth(method = "lm") 


## ----03-make-a-graph-21, echo=FALSE----------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-functionargs", "png"))


## ----reveal-logtrans, include = FALSE--------------------------------------------------------------------------------------------------------------
 p <- ggplot(data = gapminder, 
             mapping = aes(x = gdpPercap, 
                           y=lifeExp))
p + geom_point() +
    geom_smooth(method = "lm") +
    scale_x_log10()


## ----reveal-logtrans2, include = FALSE-------------------------------------------------------------------------------------------------------------
p <- ggplot(data = gapminder, 
            mapping = aes(x = gdpPercap, 
                          y=lifeExp))
p + geom_point() +
    geom_smooth(method = "lm") +
    scale_x_log10(labels = scales::label_dollar())


## ----codefig-logtranslab, message=FALSE, fig.show="hide", fig.width=5, fig.height=4.5--------------------------------------------------------------
p <- ggplot(data = gapminder, 
            mapping = aes(x = gdpPercap, 
                          y = lifeExp))
p + geom_point() + 
  geom_smooth(method = "lm") +
    scale_x_log10(labels = scales::label_dollar()) +
    labs(x = "GDP Per Capita", 
         y = "Life Expectancy in Years",
         title = "Economic Growth and Life Expectancy",
         subtitle = "Data points are country-years",
         caption = "Source: Gapminder.")


## ----03-make-a-graph-22, echo=FALSE----------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-logtranslab", "png"))


## ----03-make-a-graph-23----------------------------------------------------------------------------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = "purple"))

## Put in an object for convenience
p_out <- p + geom_point() +
    geom_smooth(method = "loess") +
    scale_x_log10()


## ----03-make-a-graph-24, fig.width=8, fig.height=5-------------------------------------------------------------------------------------------------
p_out


## ----03-make-a-graph-25----------------------------------------------------------------------------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))

## Put in an object for convenience
p_out <- p + geom_point(color = "purple") +
    geom_smooth(method = "loess") +
    scale_x_log10()


## ----03-make-a-graph-26, fig.width=8, fig.height=5-------------------------------------------------------------------------------------------------
p_out


## ----03-make-a-graph-27----------------------------------------------------------------------------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp)) 
p_out <- p + geom_point(alpha = 0.3) +
    geom_smooth(color = "orange", 
                se = FALSE, 
                size = 8, 
                method = "lm") +
    scale_x_log10()


## ----03-make-a-graph-28, fig.width=8.5, fig.height=5-----------------------------------------------------------------------------------------------
p_out


## ----reveal-pergeom1, include = FALSE--------------------------------------------------------------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent,
                          fill = continent))
p + geom_point() +
    geom_smooth(method = "loess") +
    scale_x_log10(labels = scales::label_dollar())


## ----reveal-pergeom2, include = FALSE--------------------------------------------------------------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
    geom_smooth(method = "loess") +
    scale_x_log10(labels = scales::label_dollar())
 


## ----03-make-a-graph-29, echo=TRUE, eval=FALSE-----------------------------------------------------------------------------------------------------
## 
## ## Save the most recent plot
## ggsave(filename = "figures/my_figure.png")
## 
## 
## ## Use here() for more robust file paths
## ggsave(filename = here("figures", "my_figure.png"))
## 
## ## A plot object
## p_out <- p + geom_point(mapping = aes(color = log(pop))) +
##     scale_x_log10()
## 
## ggsave(filename = here("figures", "lifexp_vs_gdp_gradient.pdf"),
##        plot = p_out)
## 
## ggsave(here("figures", "lifexp_vs_gdp_gradient.png"),
##        plot = p_out,
##        width = 8,
##        height = 5)


## ----03-make-a-graph-30, eval=FALSE, echo=TRUE-----------------------------------------------------------------------------------------------------
## knitr::opts_chunk$set(warning = TRUE,
##                         message = TRUE,
##                         fig.retina = 3,
##                         fig.align = "center",
##                         fig.asp = 0.7,
##                         dev = c("png", "pdf"))


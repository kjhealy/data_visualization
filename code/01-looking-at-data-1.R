



# Safe


## -----------------------------------------------------------------------------
#| label: "codefig-first-plot"
#| message: FALSE
#| fig.show: "hide"
#| fig.width: 5.8
#| fig.height: 5.5

p <- ggplot(data = gapminder, 
            mapping = aes(x = gdpPercap, 
                          y = lifeExp))  


p + geom_point()


## -----------------------------------------------------------------------------
#| label: "codefig-first-plot-2"
#| message: FALSE
#| fig.width: 5.8
#| fig.height: 5.5
#| output-location: column

p <- ggplot(data = gapminder, 
            mapping = aes(x = gdpPercap, 
                          y = lifeExp))  


p + geom_point()


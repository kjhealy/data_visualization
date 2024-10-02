kjh_register_tenso()    # Default fonts. Comment out if you don't have Tenso and Berkeley fonts.
kjh_set_knitr_opts()    
kjh_set_slide_theme()   # ggplot theme to go with slides. Set tenso = FALSE if necessary.





# Safe


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-1"
library(tidyverse)      # Your friend and mine
library(gapminder)      # Gapminder data
library(here)           # Portable file paths
library(socviz)         # Handy socviz functions


## -----------------------------------------------------------------------------
#| label: "reveal-groupline"
#| include: FALSE
#| output-location: column

p <- ggplot(data = gapminder, 
            mapping = aes(x = year,
                       y = gdpPercap)) + 
  geom_line()

p


## -----------------------------------------------------------------------------
#| label: "reveal-groupline2"
#| include: FALSE
#| output-location: column
p <- ggplot(data = gapminder, 
            mapping = aes(x = year,
                       y = gdpPercap)) + 
  geom_line(mapping = aes(group = country)) 

p



## -----------------------------------------------------------------------------
#| label: "reveal-facet"
#| include: FALSE
#| output-location: column
gapminder |> 
  ggplot(mapping = 
           aes(x = year,
           y = gdpPercap)) + 
  geom_line(mapping = aes(group = country)) + 
  facet_wrap(~ continent)


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-2"
p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))

p_out <- p + geom_line(color="gray70", 
              mapping=aes(group = country)) +
    geom_smooth(size = 1.1,
                method = "loess",
                se = FALSE) +
    scale_y_log10(labels=scales::label_dollar()) +
    facet_wrap(~ continent, ncol = 5) +#<<
    labs(x = "Year",
         y = "log GDP per capita",
         title = "GDP per capita on Five Continents",
         caption = "Data: Gapminder")    


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-3"
#| echo: FALSE
#| fig.width: 18
#| fig.height: 5
p_out


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-4"
midwest


## -----------------------------------------------------------------------------
#| label: "codefig-histogram"
#| message: TRUE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p <- ggplot(data = midwest, 
            mapping = aes(x = area))

p + geom_histogram()



## -----------------------------------------------------------------------------
#| label: "codefig-histogram2"
#| message: TRUE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p <- ggplot(data = midwest, 
            mapping = aes(x = area))

p + geom_histogram(bins = 10)



## -----------------------------------------------------------------------------
#| label: "codefig-subset"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
#|

## Two state codes
oh_wi <- c("OH", "WI")

midwest |> 
  filter(state %in% oh_wi) |> 
  ggplot(mapping = aes(x = percollege, 
                       fill = state)) + 
  geom_histogram(alpha = 0.5, 
                 position = "identity")



## -----------------------------------------------------------------------------
#| label: "codefig-density1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p <- ggplot(data = midwest, 
            mapping = aes(x = area))

p + geom_density()


## -----------------------------------------------------------------------------
#| label: "codefig-density2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p <- ggplot(data = midwest,
            mapping = aes(x = area, 
                          fill = state, 
                          color = state))
p + geom_density(alpha = 0.3)


## -----------------------------------------------------------------------------
#| label: "codefig-density3"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

midwest |>
  filter(state %in% oh_wi) |> 
  ggplot(mapping = aes(x = area,
                       fill = state, 
                       color = state)) + 
  geom_density(mapping = aes(y = after_stat(ndensity)), #<<
               alpha = 0.4)



## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-11"
#| echo: FALSE
#| message: FALSE
## Generate some fake data

## Keep track of labels for as_labeller() functions in plots later.
grp_names <- c(`a` = "Group A",
               `b` = "Group B",
               `c` = "Group C",
               `pop_a` = "Group A",
               `pop_b` = "Group B",  
               `pop_c` = "Group C",  
               `pop_total` = "Total",                 
               `A` = "Group A", 
               `B` = "Group B", 
               `C` = "Group C")


# make it reproducible
set.seed(1243098)

# 3,000 "counties"
N <- 3e3

## "County" populations
grp_ns <- c("size_a", "size_b", "size_c")
a_range <- c(1e5:5e5)
b_range <- c(3e5:7e5)
c_range <- c(4e5:5e5)

df_ns <- tibble(
  a_n = sample(a_range, N),
  b_n = sample(a_range, N),
  c_n = sample(a_range, N),
)


# Means and standard deviations of groups
mus <- c(0.2, 1, -0.1)
sds <- c(1.1, 0.9, 1)
grp <- c("pop_a", "pop_b", "pop_c")

# Make the parameters into a list
params <- list(mean = mus, 
                sd = sds)

# Feed the parameters to rnorm() to make three columns, 
# switch to rowwise() to take the weighted average of 
## the columns for each row.
df <- pmap_dfc(params, rnorm, n = N) |>  
  rename_with(~ grp) |> 
  rowid_to_column("unit") |> 
  bind_cols(df_ns) |> 
  rowwise() |>  
  mutate(pop_total = weighted.mean(c(pop_a, pop_b, pop_c), 
                                   w = c(a_n, b_n, c_n))) |>  
  ungroup() |> 
  select(unit:pop_c, pop_total)



## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-12"
df


## -----------------------------------------------------------------------------
#| label: "reveal-pivlongex"
#| include: FALSE
 df |> 
  pivot_longer(cols = pop_a:pop_total)


## -----------------------------------------------------------------------------
#| label: "codefig-refdist1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
df |> 
  pivot_longer(cols = pop_a:pop_total)  |> 
  ggplot() + 
  geom_histogram(mapping = aes(x = value, 
                               y = after_stat(ncount), #<<
                          color = name, fill = name), 
            stat = "bin", bins = 20, 
            linewidth = 0.5, alpha = 0.7,
            position = "identity") + 
  labs(x = "Measure", y = "Scaled Count", color = "Group", 
       fill = "Group",
       title = "Comparing Subgroups: Histograms")


## -----------------------------------------------------------------------------
#| label: "reveal-pivottrick1"
#| include: FALSE
# Treat pop_a to pop_total as a single variable
df |>
  pivot_longer(cols = pop_a:pop_total)


## -----------------------------------------------------------------------------
#| label: "reveal-pivottrick2"
#| include: FALSE
# Just treat pop_a to pop_c as the single variable.
# Notice that pop_total just gets repeated.
 df |>
  pivot_longer(cols = pop_a:pop_c)


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-14"
p_out <- df |>
  pivot_longer(cols = pop_a:pop_c) |>
  ggplot() + 
  geom_histogram(mapping = aes(x = pop_total, #<<
                               y = after_stat(ncount)), 
                bins = 20, alpha = 0.7,
                fill = "gray40", linewidth = 0.5) + 
  geom_histogram(mapping = aes(x = value, #<<
                               y = after_stat(ncount), 
                          color = name, fill = name), 
            stat = "bin", bins = 20, linewidth = 0.5,
            alpha = 0.5) + 
  guides(color = "none", fill = "none") + #<<
  labs(x = "Measure", y = "Scaled Count", 
       title = "Comparing Subgroups: Histograms", 
       subtitle = "Reference distribution shown in gray") + 
  facet_wrap(~ name, nrow = 1) 


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-15"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 5
p_out


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-16"
#| echo: FALSE
ggplot2::theme_set(ggplot2::theme_classic())


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-17"
titanic


## -----------------------------------------------------------------------------
#| label: "codefig-titanic1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p <- ggplot(data = titanic,
            mapping = aes(x = fate, 
                          y = percent, 
                          fill = sex))
p + geom_bar(stat = "identity") #<<


## -----------------------------------------------------------------------------
#| label: "codefig-titanic2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p <- ggplot(data = titanic,
            mapping = aes(x = fate, 
                          y = percent, 
                          fill = sex))
p + geom_bar(stat = "identity", 
             position = "dodge") #<<


## -----------------------------------------------------------------------------
#| label: "codefig-titanic3"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p <- ggplot(data = titanic,
            mapping = aes(x = fate, 
                          y = percent, 
                          fill = sex))
p + geom_bar(stat = "identity", 
             position = "dodge") +
  theme(legend.position = "top") #<<


## -----------------------------------------------------------------------------
#| label: "codefig-geomcol"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p <- ggplot(data = titanic,
            mapping = aes(x = fate, 
                          y = percent, 
                          fill = sex))
p + geom_col(position = "dodge") + #<<
  theme(legend.position = "top")


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-22"
#| echo: FALSE
#| message: FALSE
kjhslides::kjh_set_slide_theme()


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-23"
oecd_sum


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-24"
p <- ggplot(data = oecd_sum, 
            mapping = aes(x = year, 
                          y = diff, 
                          fill = hi_lo))

p_out <- p + geom_col() + 
  geom_hline(yintercept = 0, linewidth = 1.2) + #<<
  guides(fill = "none") + 
  labs(x = NULL, #<<
       y = "Difference in Years", 
       title = "The U.S. Life Expectancy Gap", 
       subtitle = "Difference between U.S. and 
       OECD average life expectancies, 1960-2015",
       caption = "Data: OECD.")


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-25"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 4
p_out




# Safe


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-1"
#| message: TRUE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-2"
gss_sm  


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-3"
#| echo: FALSE
gss_sm |> 
  select(bigregion, religion) |> 
  drop_na() |> 
  janitor::tabyl(bigregion, religion) |> 
  janitor::adorn_totals(where = "col") |> 
  janitor::adorn_percentages() |> 
  janitor::adorn_pct_formatting(affix_sign = FALSE) |> 
  tinytable::tt()  



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-4"
#| echo: FALSE

gss_sm |> 
  select(bigregion, religion) |> 
  drop_na() |> 
  janitor::tabyl(bigregion, religion) |> 
  janitor::adorn_totals(where = "row") |> 
  janitor::adorn_percentages(denominator = "col") |> 
  janitor::adorn_pct_formatting(affix_sign = FALSE) |> 
  tinytable::tt()  


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-5"
#| echo: FALSE

gss_sm |> 
  select(bigregion, religion) |> 
  drop_na() |> 
  janitor::tabyl(bigregion, religion) |> 
  janitor::adorn_percentages(denominator = "all") |> 
  janitor::adorn_pct_formatting(affix_sign = FALSE) |> 
  tinytable::tt()  


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-5a"
#| echo: FALSE

unfill <- function(x, blank = "") {
  x <- as.character({{x}})
  x.pos <- which(c(TRUE, x[-1]!=x[-length(x)]))
  new <- rep(blank, length(x))
  new[x.pos] <- rle(as.character(x))$values
  new
}

gss_sm |> 
  select(bigregion, race, religion) |> 
  mutate(across(where(is.factor), \(x) fct_na_value_to_level(x, level = "(Missing)"))) |> 
  count(bigregion, race, religion) |> 
  pivot_wider(names_from = bigregion, values_from = n) |> 
  mutate(across(where(is.integer), as.character)) |> 
  mutate(across(where(is.character), \(x) replace_na(x, "-"))) |> 
  mutate(race = unfill(race)) |> 
  rename_with(\(x) str_to_sentence(x)) |> 
  tinytable::tt() 



## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-3"
## library(socviz) # if not loaded
gss_sm


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-4"
gss_sm |> 
  select(id, bigregion, religion)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-5"

gss_sm |> 
  group_by(bigregion)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-6"
#| include: FALSE
gss_sm |> 
  group_by(bigregion) |> 
  summarize(total = n())


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-7"
#| include: FALSE
gss_sm |> 
  group_by(bigregion, religion) |> 
  summarize(total = n())


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-8"
#| include: FALSE
gss_sm |> 
  group_by(bigregion, religion) |> 
  summarize(total = n()) |> 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-9"
gss_sm |> 
  group_by(bigregion, religion) |> #<<
  summarize(total = n()) |> 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-10"
gss_sm |> 
  group_by(bigregion, religion) |> 
  summarize(total = n()) |> 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1)) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-11"
gss_sm |> 
  group_by(bigregion, religion) |> #<<
  summarize(total = n()) |> #<<
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1)) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-12"
gss_sm |> 
  group_by(bigregion, religion) |> #<<
  summarize(n = n()) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-13"
gss_sm |> 
  group_by(bigregion, religion) |> 
  tally() #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-14"
gss_sm |> 
  count(bigregion, religion) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-15"
#| eval: FALSE
## gss_sm |>
##   count(bigregion, religion) |>
##   pivot_wider(names_from = bigregion, values_from = n) |>  #<<
##   tinytable::tt()


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-16"
#| echo: FALSE
gss_sm |> 
  count(bigregion, religion) |> 
  pivot_wider(names_from = bigregion, values_from = n) |> 
  tinytable::tt()  


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-17"
#| fig.height: 4
#| fig.width: 15
gss_sm |> 
  group_by(bigregion, religion) |> 
  tally() |> 
  mutate(pct = round((n/sum(n))*100), 1) |> 
  drop_na() |> 
  ggplot(mapping = aes(x = pct, y = reorder(religion, -pct), fill = religion)) + #<<
  geom_col() + #<<
    labs(x = "Percent", y = NULL) +
    guides(fill = "none") + 
    facet_wrap(~ bigregion, nrow = 1)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-22"
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> 
  mutate(pct = round((n/sum(n))*100, 1)) 

rel_by_region


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-22b"
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> 
  mutate(pct = round((n/sum(n))*100, 1)) 

rel_by_region


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-23"
## Each region should sum to ~100
rel_by_region |> 
  group_by(bigregion) |> 
  summarize(total = sum(pct)) 



## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-24b"
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> #<<
  mutate(pct = round((n/sum(n))*100, 1)) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-25"
rel_by_region |> 
  summarize(total = sum(pct))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-24"
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> #<<
  mutate(pct = round((n/sum(n))*100, 1)) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-25b"
rel_by_region |> 
  summarize(total = sum(pct))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-26"
rel_by_region <- gss_sm |> 
  group_by(bigregion, religion) |> #<<
  tally() |> #<<
  mutate(pct = round((n/sum(n))*100, 1)) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-27"
# Check
rel_by_region |> 
  group_by(bigregion) |> 
  summarize(total = sum(pct))



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-24"
rel_by_region <- gss_sm |> 
  group_by(bigregion, religion) |> 
  tally() |> 
  mutate(pct = round((n/sum(n))*100, 1)) |> 
  drop_na()


head(rel_by_region)


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-25"
p <- ggplot(data = rel_by_region, 
                mapping = aes(x = bigregion, 
                              y = pct, 
                              fill = religion))
p_out <- p + geom_col(position = "dodge") +
    labs(x = "Region",
         y = "Percent", 
         fill = "Religion") 


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-26"
#| echo: FALSE
#| fig.height: 7
#| fig.width: 12
p_out


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-27"
#| echo: FALSE
#| fig.height: 7
#| fig.width: 12
p_out


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-28"
p <- ggplot(data = rel_by_region, 
                mapping = aes(x = pct, #<<
                              y = reorder(religion, -pct), #<<
                              fill = religion))
p_out_facet <- p + geom_col() +
  guides(fill = "none") + 
  facet_wrap(~ bigregion, nrow = 1) +
  labs(x = "Percent",
       y = NULL) 



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-29"
#| echo: FALSE
#| fig.height: 3.5
#| fig.width: 15
p_out_facet


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-30"
#| fig.width: 10
#| fig.height: 5
p <-  ggplot(data = gss_sm,
             mapping = aes(x = age, y = childs))

p + geom_point(alpha = 0.2) + 
  geom_smooth() +
  facet_wrap(~ race)



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-31"
#| fig.width: 8
#| fig.height: 5.5
p <-  ggplot(data = gss_sm,
             mapping = aes(x = age, y = childs))

p + geom_point(alpha = 0.2) + 
  geom_smooth() +
  facet_wrap(~ sex + race) #<<



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-32"
#| fig.width: 15
#| fig.height: 5.5
p <-  ggplot(data = gss_sm,
             mapping = aes(x = age, y = childs))

p + geom_point(alpha = 0.2) + 
  geom_smooth() +
  facet_wrap(~ sex + race, nrow = 1) #<<



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-33"
#| fig.width: 11
#| fig.height: 6.5
#| warning: FALSE
p + geom_point(alpha = 0.2) + 
  geom_smooth() +
  facet_grid(sex ~ race) #<<



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-34"
#| fig.width: 11
#| fig.height: 6.5
#| warning: FALSE
p_out <- p + geom_point(alpha = 0.2) + 
  geom_smooth() +
  facet_grid(bigregion ~ race + sex) #<<



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-35"
#| echo: FALSE
#| warning: FALSE
#| fig.width: 12
#| fig.height: 8
p_out


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-36"
gss_sm |> 
  group_by(bigregion, religion) |> 
  tally() |> 
  mutate(freq = n / sum(n),
         pct = round((freq*100), 1)) 


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-37"
organdata


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-38"
#| fig.width: 10
#| fig.height: 6
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_point()


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-39"
#| fig.width: 10
#| fig.height: 6
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line() 


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-40"
#| fig.width: 10
#| fig.height: 6
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line(aes(group = country)) 


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-41a"
#| fig.width: 21
#| fig.height: 8
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line() + 
  facet_wrap(~ country, nrow = 3)


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-41b"
#| fig.width: 21
#| fig.height: 8
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line() + 
  facet_wrap(~ reorder(country, donors, na.rm = TRUE), nrow = 3)


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-41c"
#| fig.width: 21
#| fig.height: 8
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line() + 
  facet_wrap(~ reorder(country, -donors, na.rm = TRUE), nrow = 3)


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-50"
by_country <- organdata |>  
  group_by(consent_law, country)  |> 
    summarize(donors_mean= mean(donors, na.rm = TRUE),
              donors_sd = sd(donors, na.rm = TRUE),
              gdp_mean = mean(gdp, na.rm = TRUE),
              health_mean = mean(health, na.rm = TRUE),
              roads_mean = mean(roads, na.rm = TRUE),
              cerebvas_mean = mean(cerebvas, na.rm = TRUE))

head(by_country)


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-51"
  by_country <- organdata |> 
    group_by(consent_law, country) |>
      summarize(across(where(is.numeric),#<<
                       list(mean = ~ mean(.x, na.rm = TRUE), 
                            sd = ~ sd(.x, na.rm = TRUE))))
head(by_country)              
              


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-52"
by_country <- organdata |> 
  group_by(consent_law, country) |>
    summarize(across(where(is.numeric),#<<
                       list(mean = ~ mean(.x, na.rm = TRUE), 
                            sd = ~ sd(.x, na.rm = TRUE))), 
              .groups = "drop") #<<
head(by_country)              
              


## -----------------------------------------------------------------------------
#| label: "codefig-consent1"
#| message: FALSE
#| fig.width: 8
#| fig.height: 5
#| output-location: column
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_point(size=3) +
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## -----------------------------------------------------------------------------
#| label: "codefig-consent2"
#| message: FALSE
#| fig.width: 7
#| fig.height: 7
#| output-location: column
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_point(size=3) +
  guides(color = "none") +
  facet_wrap(~ consent_law) + #<<
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## -----------------------------------------------------------------------------
#| label: "codefig-consent2a"
#| message: FALSE
#| fig.width: 5
#| fig.height: 7
#| output-location: column
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_point(size=3) +
  guides(color = "none") +
  facet_wrap(~ consent_law, ncol = 1) + #<<
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## -----------------------------------------------------------------------------
#| label: "codefig-consent3"
#| message: FALSE
#| fig.width: 7
#| fig.height: 7
#| output-location: column
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_point(size=3) +
  guides(color = "none") +
  facet_wrap(~ consent_law, 
             ncol = 1,
             scales = "free_y") +  #<<
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## -----------------------------------------------------------------------------
#| label: "codefig-consent4"
#| message: FALSE
#| fig.width: 7
#| fig.height: 7
#| output-location: column
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_pointrange(mapping = #<<
                    aes(xmin = donors_mean - donors_sd, #<<
                        xmax = donors_mean + donors_sd)) + #<<
  guides(color = "none") +
  facet_wrap(~ consent_law, 
             ncol = 1,
             scales = "free_y") +  
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-83"
#| echo: FALSE
kjhslides::kjh_set_slide_theme()


## -----------------------------------------------------------------------------
#| label: "codefig-geomtext"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

by_country |> 
  ggplot(mapping = aes(x = roads_mean, 
                       y = donors_mean)) + 
  geom_text(mapping = aes(label = country))



## -----------------------------------------------------------------------------
#| label: "codefig-geomtext2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

by_country |> 
  ggplot(mapping = aes(x = roads_mean, 
                       y = donors_mean)) + 
  geom_point() + 
  geom_text(mapping = aes(label = country),
            hjust = 0)




## -----------------------------------------------------------------------------
#| label: "codefig-geomtext3"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

by_country |> 
  ggplot(mapping = aes(x = roads_mean, 
                       y = donors_mean)) + 
  geom_point() + 
  geom_text(mapping = aes(x = roads_mean + 2, 
                          label = country),
            hjust = 0)




## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-61"
elections_historic


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-62"
## The packages we'll use in addition to ggplot
library(ggrepel) #<<
library(scales) #<<

p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional."
x_label <- "Winner's share of Popular Vote"
y_label <- "Winner's share of Electoral College Votes"


## -----------------------------------------------------------------------------
#| label: "codefig-presplot1"
#| message: FALSE
#| fig.width: 5
#| fig.height: 4.5
#| output-location: column
p <- ggplot(data = elections_historic, 
            mapping = aes(x = popular_pct, 
                          y = ec_pct,
                          label = winner_label))

p + geom_hline(yintercept = 0.5, 
               linewidth = 1.4, 
               color = "gray80") +
    geom_vline(xintercept = 0.5, 
               linewidth = 1.4, 
               color = "gray80") +
    geom_point()



## -----------------------------------------------------------------------------
#| label: "codefig-presplot2"
#| message: FALSE
#| fig.width: 5
#| fig.height: 4.5
#| output-location: column
p <- ggplot(data = elections_historic, 
            mapping = aes(x = popular_pct, 
                          y = ec_pct,
                          label = winner_label))

p + geom_hline(yintercept = 0.5, 
               linewidth = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, 
             linewidth = 1.4, color = "gray80") +
  geom_point() + 
  geom_text_repel()



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-65"
p <- ggplot(data = elections_historic, 
            mapping  = aes(x = popular_pct, 
                           y = ec_pct,
                           label = winner_label))

p_out <- p + 
  geom_hline(yintercept = 0.5, 
             linewidth = 1.4, 
             color = "gray80") +
  geom_vline(xintercept = 0.5, 
             linewidth = 1.4, 
             color = "gray80") +
  geom_point() + 
  geom_text_repel() #<<



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-66"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_out


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-67"
p <- ggplot(data = elections_historic, 
            mapping  = aes(x = popular_pct, 
                           y = ec_pct,
                           label = winner_label))
p_out <- p + geom_hline(yintercept = 0.5, 
                        linewidth = 1.4, 
                        color = "gray80") +
    geom_vline(xintercept = 0.5, 
               linewidth = 1.4, 
               color = "gray80") +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(labels = label_percent()) + #<<
    scale_y_continuous(labels = label_percent()) #<<


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-68"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_out


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-69"
p <- ggplot(data = elections_historic, 
            mapping  = aes(x = popular_pct, 
                           y = ec_pct,
                           label = winner_label))
p_out <- p + geom_hline(yintercept = 0.5, 
                        linewidth = 1.4, 
                        color = "gray80") +
  geom_vline(xintercept = 0.5, 
             linewidth = 1.4, 
             color = "gray80") +
  geom_point() +
  geom_text_repel(mapping = aes(family = "Tenso Slide")) +#<<
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = x_label, y = y_label,  #<<
       title = p_title, 
       subtitle = p_subtitle,
       caption = p_caption)   
  
  
  


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-70"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_out


## -----------------------------------------------------------------------------
#| label: "codefig-subset1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
by_country |> 
  ggplot(mapping = aes(x = gdp_mean,
                       y = health_mean)) +
  geom_point() + 
  geom_text_repel(data = subset(by_country, gdp_mean > 25000), 
                  mapping = aes(label = country))




## -----------------------------------------------------------------------------
#| label: "codefig-subset2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

by_country |> 
  ggplot(mapping = aes(x = gdp_mean,
                       y = health_mean)) +
  geom_point() + 
  geom_text_repel(data = subset(by_country, 
                                gdp_mean > 25000 |
                                  health_mean < 1500 |
                                  country %in% "Belgium"), 
                  mapping = aes(label = country))




## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-73"
df_hl <- by_country |> 
  filter(gdp_mean > 25000 | 
           health_mean < 1500 | 
           country %in% "Belgium")

df_hl


## -----------------------------------------------------------------------------
#| label: "codefig-subset3"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

by_country |> 
  ggplot(mapping = aes(x = gdp_mean,
                       y = health_mean)) +
  geom_point() + 
  geom_text_repel(data = df_hl, 
                  mapping = aes(label = country))




## -----------------------------------------------------------------------------
#| label: "codefig-annotate1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

organdata |> 
  ggplot(mapping = aes(x = roads, 
                       y = donors)) + 
  geom_point() + 
  annotate(geom = "text", 
           family = "Tenso Slide",
           x = 157, 
           y = 33,
           label = "A surprisingly high \n recovery rate.",
           hjust = 0)



## -----------------------------------------------------------------------------
#| label: "codefig-annotate2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

organdata |> 
  ggplot(mapping = aes(x = roads, 
                       y = donors)) + 
  geom_point() +
  annotate(geom = "rect", 
           xmin = 125, xmax = 155,
           ymin = 30, ymax = 35,
           fill = "red", 
           alpha = 0.2) + 
  annotate(geom = "text", 
           x = 157, y = 33,
           family = "Tenso Slide",
           label = "A surprisingly high \n recovery rate.", 
           hjust = 0)



## -----------------------------------------------------------------------------
#| label: "codefig-scalefn1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

organdata |> 
  ggplot(mapping = aes(x = roads,
                       y = donors,
                       color = world)) + 
  geom_point() +
  scale_y_continuous(breaks = c(5, 15, 25),
                     labels = c("Five", 
                                "Fifteen", 
                                "Twenty Five"))



## -----------------------------------------------------------------------------
#| label: "codefig-scalecolordiscrete"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

organdata |> 
  ggplot(mapping = aes(x = roads,
                       y = donors,
                       color = world)) + 
  geom_point() +
  scale_color_discrete(labels =
                         c("Corporatist", 
                           "Liberal",
                           "Social Democratic", 
                           "Unclassified")) +
  labs(x = "Road Deaths",
       y = "Donor Procurement",
       color = "Welfare State")



## -----------------------------------------------------------------------------
#| label: "codefig-guidesfn"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 5.5
#| output-location: column

organdata |> 
  ggplot(mapping = aes(x = roads,
                       y = donors,
                       color = consent_law)) + 
  geom_point() +
  facet_wrap(~ consent_law, ncol = 1) +
  guides(color = "none") + 
  labs(x = "Road Deaths",
       y = "Donor Procurement")



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-81"
#| echo: FALSE
kjhslides::kjh_set_classic_theme(4)


## -----------------------------------------------------------------------------
#| label: "codefig-themefn"
#| message: FALSE
#| fig.width: 5
#| fig.height: 4.5
#| output-location: column
## Using the "classic" ggplot theme here
organdata |> 
  ggplot(mapping = aes(x = roads,
                       y = donors,
                       color = consent_law)) + 
  geom_point() +
  labs(title = "By Consent Law",
    x = "Road Deaths",
    y = "Donor Procurement", 
    color = "Legal Regime:") + 
  theme(legend.position = "bottom", 
        plot.title = element_text(color = "darkred",
                                  face = "bold"))



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-83-2"
#| echo: FALSE
kjhslides::kjh_set_slide_theme()


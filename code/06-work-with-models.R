kjh_register_tenso()    # Default fonts. Comment out if you don't have Tenso and Berkeley fonts.
kjh_set_knitr_opts()    
kjh_set_slide_theme()   # ggplot theme to go with slides. Set tenso = FALSE if necessary.




## -----------------------------------------------------------------------------
#| label: "06-work-with-models-1"
#| message: TRUE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine
library(gapminder) # Everyone's favorite dataset
library(broom)     # Tidy model output
library(marginaleffects) # Tidy marginal effects
library(modelsummary) # Tidy summary tables and graphs
library(scales)    # Format our axes and guides


## -----------------------------------------------------------------------------
#| label: "codefig-model1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p <- gapminder |> 
  ggplot(mapping = aes(x = log(gdpPercap), 
                       y = lifeExp))  

p + geom_point(alpha=0.1) +
  geom_smooth(color = "tomato", 
              fill="tomato", 
              method = MASS::rlm) +
  geom_smooth(color = "steelblue", 
              fill="steelblue", 
              method = "lm") + 
  labs(title = "Robust and OLS fits")


## -----------------------------------------------------------------------------
#| label: "codefig-model2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p + geom_point(alpha=0.1) +
    geom_smooth(color = "tomato", 
        method = "lm", 
        size = 1.2, 
        formula = y ~ splines::bs(x, 3), 
        se = FALSE)



## -----------------------------------------------------------------------------
#| label: "codefig-model3"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p + geom_point(alpha=0.1) +
    geom_quantile(color = "tomato", 
        size = 1.2, 
        method = "rqss",
        lambda = 1, 
        quantiles = c(0.20, 0.5, 0.85))



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-5"
gapminder


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-6"
out <- lm(formula = lifeExp ~ gdpPercap + log(pop) + continent, 
          data = gapminder)

summary(out)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-7"
min_gdp <- min(gapminder$gdpPercap)
max_gdp <- max(gapminder$gdpPercap)
med_pop <- median(gapminder$pop)

# Make a grid of predictor values
pred_df <- expand_grid(gdpPercap = (seq(from = min_gdp,
                                        to = max_gdp,
                                        length.out = 100)),
                       pop = med_pop,
                       continent = c("Africa", "Americas",
                                     "Asia", "Europe", "Oceania"))

pred_df


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-8"
# Get the predicted values
pred_out <- predict(object = out,
                    newdata = pred_df,
                    interval = "confidence")
head(pred_out)



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-9"
# Bind them into one data frame. We can do this safely
# here because we know the row order by construction. 
# But this is not a safe approach in general.

pred_df <- cbind(pred_df, pred_out)
head(pred_df)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-10"
p <- ggplot(data = subset(pred_df, continent %in% c("Europe", "Africa")),
            aes(x = gdpPercap,
                y = fit, 
                ymin = lwr, 
                ymax = upr,
                color = continent,
                fill = continent,
                group = continent))

# Use the original data as the point layer
p_out <- p + geom_point(data = subset(gapminder,
                             continent %in% c("Europe", "Africa")),
               mapping = aes(x = gdpPercap, y = lifeExp,
                   color = continent),
               alpha = 0.5,
               inherit.aes = FALSE) + 
# And the predicted values to draw the lines  
    geom_line() +
    geom_ribbon(alpha = 0.2, color = FALSE) +
    scale_x_log10(labels = scales::label_dollar())


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-11"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 7
p_out


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-12"
out <- lm(formula = lifeExp ~ gdpPercap + log(pop) + continent, 
          data = gapminder)

summary(out)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-13"
library(broom)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-14"
tidy(out)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-15"
out_conf <- tidy(out, conf.int = TRUE)
out_conf 


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-16"
out_conf |> 
    filter(term %nin% "(Intercept)")  |> 
    mutate(nicelabs = prefix_strip(term, "continent")) |> 
    relocate(nicelabs)


## -----------------------------------------------------------------------------
#| label: "codefig-broomplot"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
out_conf |> 
    filter(term %nin% "(Intercept)") |> 
    mutate(nicelabs = prefix_strip(term, "continent")) |> 
  ggplot(mapping = aes(x = estimate, 
                       xmin = conf.low, 
                       xmax = conf.high, 
                       y = reorder(nicelabs, 
                                   estimate))) + 
  geom_pointrange() + 
  labs(x = "Estimate", 
       y = NULL, 
       title = "Severely Misspecified")


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-18"
augment(out)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-19"
# Adding the data argument puts back any additional columns from the original
# tibble
out_aug <-  augment(out, data = gapminder)
head(out_aug)



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-20"
## Residuals vs Fitted Values
p <- ggplot(data = out_aug,
            mapping = aes(x = .fitted, y = .resid))
p_out <- p + geom_point() 


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-21"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8
p_out


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-22"
glance(out)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-23"
library(survival)


head(lung)

tail(lung)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-24"
## Hazard model
out_cph <- coxph(Surv(time, status) ~ age + sex, data = lung)

summary(out_cph)



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-25"
## Hazard model
out_surv <- survfit(out_cph)

## See how this is just a print method,
## not a tibble, or even a data frame.
## So it just runs off the end of the slide.
summary(out_surv)



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-26"
## Much nicer. (See how the column headers have been regularized, too.)
out_tidy <- tidy(out_surv)
out_tidy

p_out <- out_tidy |> 
  ggplot(mapping = aes(x = time, y = estimate)) + 
  geom_line() + 
  geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high),#<<
              alpha = 0.4) #<<



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-27"
#| echo: FALSE
#| fig.height: 8
#| fig.width: 15
p_out


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-28"
x <- 10

for (i in 1:5) {
  print(x + i)
}


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-29"
x <- c(10, 20, 30, 40)

for (i in 1:length(x)) {
  # Add 5 to the ith element of x
  print(x[i] + 5)
}


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-30"
x <- 10

for (i in 1:5) {
  print(sqrt(x + i))
}


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-31"
a <- c(1:10)

b <- 1

# You know what R will do here
a + b



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-31a"
a <- c(1:10)

b <- 1

# You know what R will do here
a + b



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-32"
a <- c(1:10)


add_b <- function(x) {
  b <- 1
  x + b # for any x
}


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-33"
add_b(x = a)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-34"
add_b(x = 10)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-35"
add_b(x = c(1, 99, 1000))


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-36"
library(gapminder)
gapminder |>  
  summarize(country_n = n_distinct(country), 
            continent_n = n_distinct(continent), 
            year_n = n_distinct(year), 
            lifeExp_n = n_distinct(lifeExp), 
            population_n = n_distinct(population))


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-37"
library(gapminder)
gapminder |>  
  summarize(n_distinct(country), 
            n_distinct(continent), 
            n_distinct(year), 
            n_distinct(lifeExp), 
            n_distinct(population))


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-38"
gapminder |>  
  summarize(across(everything(), n_distinct))


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-39"
  map(gapminder, n_distinct)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-40"
gapminder |>  
  map(n_distinct)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-41"
result <- gapminder |>  
  map(n_distinct)

class(result)

result$continent

result[[2]]


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-42"
gapminder |>  
  map_int(n_distinct)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-43"
eu77 <- gapminder |> 
  filter(continent == "Europe", year == 1977)

fit <- lm(lifeExp ~ log(gdpPercap), data = eu77)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-44"
summary(fit)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-45"

out_le <- gapminder |> group_by(continent, year) |>
    nest() #<<

out_le



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-46"
out_le |> 
  filter(continent == "Europe" & year == 1977) |> 
  unnest(cols = c(data))


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-47"
#| echo: FALSE
old_digits <- getOption("digits")
options(digits = 3)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-48"

fit_ols <- function(df) {
    lm(lifeExp ~ log(gdpPercap), data = df)
}

out_le <- gapminder |>
    group_by(continent, year) |>
    nest() |> 
    mutate(model = map(data, fit_ols)) #<<


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-49"
out_le


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-50"

fit_ols <- function(df) {
    lm(lifeExp ~ log(gdpPercap), data = df)
}

out_tidy <- gapminder |>
    group_by(continent, year) |>
    nest() |> 
    mutate(model = map(data, fit_ols),
           tidied = map(model, tidy)) 

out_tidy


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-51"
out_tidy <- out_tidy |>
    unnest(cols = c(tidied)) |>
    filter(term %nin% "(Intercept)" &
           continent %nin% "Oceania")

out_tidy


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-52"
p <- ggplot(data = out_tidy,
            mapping = aes(x = year, y = estimate,
                          ymin = estimate - 2*std.error,
                          ymax = estimate + 2*std.error,
                          group = continent, 
                          color = continent))

p_out <- p + 
  geom_pointrange(size = rel(1.25), 
                  position = position_dodge(width = rel(1.3))) +#<<
  scale_x_continuous(breaks = unique(gapminder$year)) + #<<
  labs(x = "Year", 
       y = "Estimate", 
       color = "Continent")


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-53"
#| echo: FALSE
#| fig.height: 6
#| fig.width: 15
p_out


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-54"
# New model
fit_ols2 <- function(df) {
    lm(lifeExp ~ log(gdpPercap) + log(pop), data = df)
}

out_tidy <- gapminder |>
    group_by(continent, year) |>
    nest() |> 
    mutate(model = map(data, fit_ols2),
           tidied = map(model, tidy)) 

out_tidy


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-55"
# Plot the output from our model
mod_plot <- function(data, 
                     title){
  data |> 
    filter(term %nin% "(Intercept)") |> 
    ggplot(mapping = aes(x = estimate,
                         xmin = estimate - std.error,
                         xmax = estimate + std.error,
                         y = reorder(term, estimate))) + 
    geom_pointrange() + 
    labs(title = title, 
         y = NULL)
}


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-56"
out_tidy <- gapminder |> group_by(continent, year) |> nest() |> 
    mutate(title = paste(continent, year),
           model = map(data, fit_ols2),#<<
           tidied = map(model, tidy), 
           ggout = pmap(list(tidied, title), #<<
                        mod_plot)) #<<

out_tidy


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-57"
#| fig.height: 3
#| fig.width: 6
out_tidy$ggout[[8]]


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-58"
#| fig.height: 3
#| fig.width: 6
out_tidy$ggout[[18]]


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-59"

pwalk(
  list(
    filename = paste0(out_tidy$title, ".png"),
    plot = out_tidy$ggout,
    path = here("figures"),
    height = 3, width = 4,
    dpi = 300
  ),
  ggsave
)


                       


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-60"
fs::dir_ls(here("figures")) |> 
  basename()


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-61"
gss_sm


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-62"
gss_sm$polviews_m <- relevel(gss_sm$polviews, 
                             ref = "Moderate")

out_bo <- glm(obama ~ polviews_m + sex*race,
              family = "binomial", 
              data = gss_sm)

tidy(out_bo)



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-63"
library(marginaleffects)

bo_mfx <- avg_slopes(out_bo)

## This gives us the marginal effects at the unit level 
as_tibble(bo_mfx)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-65"
tidy(bo_mfx)


## -----------------------------------------------------------------------------
#| label: "codefig-meffects"
#| message: FALSE
#| fig.width: 7
#| fig.height: 3.5
#| output-location: column

tidy(bo_mfx) |> 
  ggplot(mapping = aes(x = estimate, 
                       xmin = conf.low, 
                       xmax = conf.high,
                       y = reorder(contrast, 
                                   estimate))) + 
  geom_vline(xintercept = 0, color = "gray70",
             size = rel(1.2)) +
  geom_pointrange() +
  labs(x = "Average Marginal Effect", 
       y = NULL)



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-67"
## Load the packages
library(survey)
library(srvyr)


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-68"
gss_lon


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-69"
# These details are dependent on the kind of survey you're working with
options(survey.lonely.psu = "adjust")
options(na.action="na.pass")

gss_svy <- gss_lon |> 
  filter(year > 1974) |> 
  mutate(stratvar = interaction(year, vstrat)) |> 
  as_survey_design(ids = vpsu,
                   strata = stratvar,
                   weights = wtssall,
                   nest = TRUE)

gss_svy # Now it's no longer simply a tibble


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-70"
out_hap <- gss_svy |> 
    group_by(year, happy)  |> 
    summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))

out_hap


## -----------------------------------------------------------------------------
#| label: "codefig-gssaltfig"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
out_hap |>  
  filter(happy == "Not Too Happy") |> 
  ggplot(mapping = aes(x = year, 
                       y = prop,
                       ymin = prop_low, 
                       ymax = prop_upp)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(alpha = 0.3) +
  scale_x_continuous(breaks = 
                       seq(1978, 2018, 4)) +
  scale_y_continuous(labels = 
                       label_percent(accuracy = 1)) + 
  labs(x = "Year",
    y = "Percent",
    title = "Trends in Unhappiness", 
    subtitle = "1975-2018", 
    caption = "Data: GSS.")



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-72"
#| echo: FALSE
#| fig.width: 11
#| fig.height: 5
out_hap |>
  filter(happy == "Not Too Happy") |> 
  ggplot(mapping = aes(x = year, 
                       y = prop,
                       ymin = prop_low, 
                       ymax = prop_upp)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(alpha = 0.3) +
  scale_x_continuous(breaks = 
                       seq(1978, 2018, 4)) +
  scale_y_continuous(labels = 
                       label_percent(accuracy = 1)) + 
  labs(x = "Year",
    y = "Percent",
    title = "Trends in Unhappiness", 
    subtitle = "1975-2018", 
    caption = "Data: GSS.")


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-73"
gss_svy |>
  filter(year %in% seq(1976, 2016, by = 4)) |>
  group_by(year, race, degree) |>
  summarize(prop = survey_mean(na.rm = TRUE)) 


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-74"

out_yrd <- gss_svy |>
  filter(year %in% seq(1976, 2016, by = 4)) |>
  group_by(year, race, degree) |>
  summarize(prop = survey_mean(na.rm = TRUE)) 



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-75"
out_yrd |> 
  group_by(year, race) |> 
  summarize(tot = sum(prop))


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-76"
p <- out_yrd |> 
  drop_na() |> 
  filter(race %nin% "Other") |> 
  ggplot(mapping = aes(x = degree, 
                       y = prop,
                       ymin = prop - 2*prop_se,
                       ymax = prop + 2*prop_se,
                       fill = race,
                       color = race,
                       group = race))

dodge_w <- position_dodge(width = 0.9)


## -----------------------------------------------------------------------------
#| label: "codefig-barplotreplace"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p + geom_col(position = dodge_w, alpha = 0.2) +
    geom_errorbar(position = dodge_w, width = 0.2) +
    scale_x_discrete(labels = wrap_format(10)) +
    scale_y_continuous(labels = label_percent()) +
    scale_color_brewer(type = "qual", 
                       palette = "Dark2") +
    scale_fill_brewer(type = "qual", 
                      palette = "Dark2") +
    labs(title = "Educational Attainment by Race",
         subtitle = "GSS 1976-2016",
         fill = "Race",
         color = "Race",
         x = NULL, y = "Percent") +
    facet_wrap(~ year, ncol = 2) 



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-78"
p_out <- p + 
  geom_col(position = dodge_w, alpha = 0.2) +
  geom_errorbar(position = dodge_w, width = 0.2) +
  scale_x_discrete(labels = wrap_format(10)) +
  scale_y_continuous(labels = label_percent()) +
  scale_color_brewer(type = "qual", 
                     palette = "Dark2") +
  scale_fill_brewer(type = "qual", 
                    palette = "Dark2") +
  labs(title = "Educational Attainment by Race",
       subtitle = "GSS 1976-2016",
       fill = "Race",
       color = "Race",
       x = NULL, y = "Percent") +
  facet_wrap(~ year, nrow = 2) + 
  theme(axis.text.x = 
          element_text(size = rel(0.6), 
                       face = "bold"))


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-79"
#| echo: FALSE
#| fig.width: 18
#| fig.height: 6
p_out


## -----------------------------------------------------------------------------
#| label: "06-work-with-models-80"
p <- out_yrd |> 
  drop_na() |> 
  filter(race %nin% "Other", 
         degree %nin% "Junior College") |> 
  ggplot(mapping = aes(x = year, y = prop, 
                          ymin = prop - 2*prop_se,
                          ymax = prop + 2*prop_se, 
                          fill = race, color = race,
                          group = race))

p_out <- p + 
  geom_ribbon(mapping = aes(color = NULL),
              alpha = 0.3) +
  geom_line(linewidth = rel(1.25)) + 
  scale_y_continuous(labels = label_percent()) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(~ degree, ncol = 2) +
  labs(title = "Educational Attainment by Race",
       subtitle = "GSS 1976-2016", fill = "Race",
       color = "Race", x = NULL, y = "Percent") 



## -----------------------------------------------------------------------------
#| label: "06-work-with-models-81"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
p_out


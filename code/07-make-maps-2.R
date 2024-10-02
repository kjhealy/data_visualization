kjh_register_tenso()    # Default fonts. Comment out if you don't have Tenso and Berkeley fonts.
kjh_set_knitr_opts()    
kjh_set_slide_theme()   # ggplot theme to go with slides. Set tenso = FALSE if necessary.




## -----------------------------------------------------------------------------
#| label: "07-make-maps-1"
#| message: FALSE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine
library(maps)      # Some basic maps
library(sf)        # Simple Features Geometries and geom_sf()
library(ggforce)   # Useful enhancements to ggplot


## -----------------------------------------------------------------------------
#| label: "07-make-maps-49"
#| echo: FALSE
detach(package:sf)


## -----------------------------------------------------------------------------
#| label: "07-make-maps-50"
#| message: TRUE
library(sf)


## -----------------------------------------------------------------------------
#| label: "07-make-maps-51"
library(nycdogs)
nyc_license


## -----------------------------------------------------------------------------
#| label: "07-make-maps-52"
nyc_zips


## -----------------------------------------------------------------------------
#| label: "07-make-maps-53"
nyc_zips |> 
  select(objectid:borough)


## -----------------------------------------------------------------------------
#| label: "reveal-nycfrench"
#| include: FALSE
nyc_license  |> 
  filter(extract_year == 2018) |> 
  group_by(breed_rc, zip_code) |> 
  tally() |> 
  mutate(freq = n / sum(n)) |> 
  filter(breed_rc == "French Bulldog") ->
  nyc_fb



## -----------------------------------------------------------------------------
#| label: "make_nyc_fb"
#| echo: FALSE
nyc_license  |> 
  filter(extract_year == 2018) |> 
  group_by(breed_rc, zip_code) |> 
  tally() |> 
  mutate(freq = n / sum(n)) |> 
  filter(breed_rc == "French Bulldog") ->
  nyc_fb


## -----------------------------------------------------------------------------
#| label: "save_nyc_fb"
#| echo: FALSE
save(nyc_fb, file = "nyc_fb.Rdata")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-54"
nyc_zips |> select(objectid:st_fips)


## -----------------------------------------------------------------------------
#| label: "07-make-maps-55"
nyc_fb |> select(breed_rc:n)


## -----------------------------------------------------------------------------
#| label: "07-make-maps-56"
fb_map <- left_join(nyc_zips, nyc_fb, by = "zip_code")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-57"
fb_map |> select(zip_code, po_name, borough, breed_rc:freq, geometry)


## -----------------------------------------------------------------------------
#| label: "save_fb_map"
#| echo: FALSE
#| message: FALSE
save(fb_map, file = "fb_map.Rdata")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-58"
theme_nymap <- function(base_size=9, base_family="") {
    require(grid)
    theme_bw(base_size=base_size, base_family=base_family) %+replace%
        theme(axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank(),
              panel.spacing=unit(0, "lines"),
              plot.background=element_blank(),
              legend.justification = c(0,0),
              legend.position = c(0.05, 0.58), 
              legend.direction = "horizontal"
        )
}



## -----------------------------------------------------------------------------
#| label: bulldog-map-manual-1
#| output-location: column

fb_map |> 
  ggplot(mapping = aes(fill = freq)) +
  geom_sf(color = "gray30", size = 0.1)



## -----------------------------------------------------------------------------
#| label: bulldog-map-manual-2
#| output-location: column

fb_map |> 
  ggplot(mapping = aes(fill = freq)) +
  geom_sf(color = "gray30", size = 0.1) + #<<
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Percent of All French Bulldogs") 


## -----------------------------------------------------------------------------
#| label: bulldog-map-manual-3
#| output-location: column

fb_map |> 
  ggplot(mapping = aes(fill = freq)) +
  geom_sf(color = "gray30", size = 0.1) + #<<
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Percent of All French Bulldogs") +
  annotate(geom = "text", 
             x = -74.145 + 0.029, 
             y = 40.82-0.012, 
           label = "New York City's French Bulldogs", 
           size = 6) + 
    annotate(geom = "text", 
             x = -74.1468 + 0.029, 
             y = 40.8075-0.012, 
           label = "By Zip Code. Based on Licensing Data", 
           size = 5) 



## -----------------------------------------------------------------------------
#| label: bulldog-map-manual-4
#| output-location: column

fb_map |> 
  ggplot(mapping = aes(fill = freq)) +
  geom_sf(color = "gray30", size = 0.1) + #<<
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Percent of All French Bulldogs") +
  annotate(geom = "text", 
             x = -74.145 + 0.029, 
             y = 40.82-0.012, 
           label = "New York City's French Bulldogs", 
           size = 6) + 
  annotate(geom = "text", 
             x = -74.1468 + 0.029, 
             y = 40.8075-0.012, 
           label = "By Zip Code. Based on Licensing Data", 
           size = 5) + 
  kjhslides::kjh_theme_nymap() + 
  guides(fill = 
           guide_legend(title.position = "top", 
                label.position = "bottom",
                keywidth = 1, 
                nrow = 1))  



## -----------------------------------------------------------------------------
#| label: "reveal-bulldogmap-slide"
#| echo: false
#| fig.width: 12
#| fig.height: 8

load("fb_map.Rdata")
fb_map |> 
    ggplot(mapping = aes(fill = freq)) +
    geom_sf(color = "gray30", size = 0.1) + #<<
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Percent of All French Bulldogs") +
    annotate(geom = "text", 
             x = -74.145 + 0.029, 
             y = 40.82-0.012, 
           label = "New York City's French Bulldogs", 
           size = 6) + 
    annotate(geom = "text", 
             x = -74.1468 + 0.029, 
             y = 40.8075-0.012, 
           label = "By Zip Code. Based on Licensing Data", 
           size = 5) + 
    kjhslides::kjh_theme_nymap() + 
   guides(fill = 
           guide_legend(title.position = "top", 
                label.position = "bottom",
                keywidth = 1, 
                nrow = 1))  
 


## -----------------------------------------------------------------------------
#| label: "reveal-bulldogmap2-1"
#| output-location: column
library(colorspace)

fb_map |> 
  ggplot(mapping = aes(fill = freq)) + 
  geom_sf(color = "gray30", size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Oranges",
    labels = scales::label_percent()) +
  labs(fill = "Percent of all French Bulldogs") 


## -----------------------------------------------------------------------------
#| label: "reveal-bulldogmap2-2"
#| output-location: column

fb_map |> 
  ggplot(mapping = aes(fill = freq)) + 
  geom_sf(color = "gray30", size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Oranges",
    labels = scales::label_percent()) +
  labs(fill = "Percent of all French Bulldogs") +
  annotate(geom = "text", 
           x = -74.145 + 0.029, 
           y = 40.82-0.012, 
           label = "New York City's French Bulldogs", 
           size = 6) + 
  annotate(geom = "text", 
           x = -74.1468 + 0.029, 
           y = 40.7955, 
           label = "By Zip Code. Based on Licensing Data", 
           size = 5) + 
  kjhslides::kjh_theme_nymap() + 
  guides(fill = 
           guide_legend(title.position = "top", 
                label.position = "bottom",
                keywidth = 1, 
                nrow = 1))  



## -----------------------------------------------------------------------------
#| label: "07-make-maps-59"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8

fb_map |> 
  ggplot(mapping = aes(fill = freq)) + 
  geom_sf(color = "gray30", size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Oranges",
    labels = scales::label_percent()) +
  labs(fill = "Percent of all French Bulldogs") +
  annotate(geom = "text", 
           x = -74.145 + 0.029, 
           y = 40.82-0.012, 
           label = "New York City's French Bulldogs", 
           size = 6) + 
  annotate(geom = "text", 
           x = -74.1468 + 0.029, 
           y = 40.7955, 
           label = "By Zip Code. Based on Licensing Data", 
           size = 5) + 
  kjhslides::kjh_theme_nymap() + 
  guides(fill = 
           guide_legend(title.position = "top", 
                label.position = "bottom",
                keywidth = 1, 
                nrow = 1))  



## -----------------------------------------------------------------------------
#| label: "reveal-bulldogzeros"

nyc_license  |> 
  filter(extract_year == 2018) |> 
  group_by(breed_rc, zip_code) |> 
  tally() |> 
  ungroup() |> 
  complete(zip_code, breed_rc, 
             fill = list(n = 0)) |> 
  # Regroup to get the right denominator
  group_by(breed_rc) |> 
  mutate(freq = n / sum(n)) |> 
  filter(breed_rc == "French Bulldog") ->
  nyc_fb2

fb_map2 <- left_join(nyc_zips, 
                     nyc_fb2, 
                     by = "zip_code")



## -----------------------------------------------------------------------------
#| echo: FALSE
save(fb_map2, nyc_fb2, file = "fbnyc2.Rdata")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-60"
fb_map2 |> select(zip_code, po_name, borough, breed_rc:freq, geometry)


## -----------------------------------------------------------------------------
#| label: "reveal-bulldogfb2-1"
#| output-location: column

fb_map2 |>
  ggplot(mapping = aes(fill = freq)) + 
  geom_sf(color = "gray30", size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Oranges", 
    labels = scales::label_percent()) 


## -----------------------------------------------------------------------------
#| label: "reveal-bulldogfb2-2"
#| output-location: column

fb_map2 |>
  ggplot(mapping = aes(fill = freq)) + 
  geom_sf(color = "gray30", size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Oranges", 
    labels = scales::label_percent()) +
  labs(fill = "Percent of all French Bulldogs") +
  annotate(geom = "text", 
           x = -74.145 + 0.029, 
           y = 40.82-0.012, 
           label = "New York City's French Bulldogs", 
           size = 6) + 
  annotate(geom = "text", 
           x = -74.1468 + 0.029, 
           y = 40.7955, 
           label = "By Zip Code. Based on Licensing Data", 
           size = 5) + 
  kjhslides::kjh_theme_nymap() + 
  guides(fill = 
           guide_legend(title.position = "top", 
                        label.position = "bottom",
                        keywidth = 1, 
                        nrow = 1))  



## -----------------------------------------------------------------------------
#| label: "07-make-maps-61"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
fb_map2 |> 
  ggplot(mapping = aes(fill = freq)) +
  geom_sf(color = "gray30", size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Oranges", 
    labels = scales::label_percent()) +
  labs(fill = "Percent of all French Bulldogs") +
  annotate(geom = "text", 
           x = -74.145 + 0.029, 
           y = 40.808, 
           label = "New York City's French Bulldogs", 
           size = 6) + 
  annotate(geom = "text", 
           x = -74.145 + 0.028, 
           y = 40.795, 
           label = "By Zip Code. Based on Licensing Data", 
           size = 5) + 
  kjhslides::kjh_theme_nymap() + 
  guides(fill = 
           guide_legend(title.position = "top", 
                        label.position = "bottom",
                        keywidth = 1, 
                        nrow = 1))  



## -----------------------------------------------------------------------------
# install.packages("cartogram")
library(cartogram)
options(tigris_use_cache = TRUE)


## -----------------------------------------------------------------------------
pop_names <- tribble(
    ~varname, ~clean,
    "B01003_001", "pop",
    "B01001B_001", "black",
    "B01001A_001", "white",
    "B01001H_001", "nh_white",
    "B01001I_001", "hispanic",
    "B01001D_001", "asian"
  )
  
pop_names



## -----------------------------------------------------------------------------
library(tidycensus)
fips_pop <- get_acs(geography = "county", 
                    variables = pop_names$varname, 
                    cache_table = TRUE) |>  
  left_join(pop_names, join_by(variable == varname)) |> 
  mutate(variable = clean) |> 
  select(-clean, -moe) |>
  pivot_wider(names_from = variable, values_from = estimate) |> 
  rename(fips = GEOID, name = NAME) |> 
  mutate(prop_pop = pop/sum(pop), 
         prop_black = black/pop, 
         prop_hisp = hispanic/pop, 
         prop_white = white/pop, 
         prop_nhwhite = nh_white/pop, 
         prop_asian = asian/pop)

fips_map <- get_acs(geography = "county", 
                    variables = "B01001_001", 
                    geometry = TRUE,
                    shift_geo = FALSE,
                    cache_table = TRUE) |> 
  select(GEOID, NAME, geometry) |> 
  rename(fips = GEOID, name = NAME)


## -----------------------------------------------------------------------------
pop_cat_labels <- c("<5", as.character(seq(10, 95, 5)), "100")

counties_sf <- fips_map |>
  left_join(fips_pop, by = c("fips", "name")) |> 
  mutate(black_disc = cut(prop_black*100,
                         breaks = seq(0, 100, 5),
                         labels = pop_cat_labels,
                         ordered_result = TRUE), 
         hisp_disc = cut(prop_hisp*100,
                         breaks = seq(0, 100, 5),
                         labels = pop_cat_labels,
                         ordered_result = TRUE), 
         nhwhite_disc = cut(prop_nhwhite*100,
                         breaks = seq(0, 100, 5),
                         labels = pop_cat_labels,
                         ordered_result = TRUE),
        asian_disc = cut(prop_asian*100,
                         breaks = seq(0, 100, 5),
                         labels = pop_cat_labels,
                         ordered_result = TRUE)) |>
  sf::st_transform(crs = 2163)



## -----------------------------------------------------------------------------
counties_sf


## -----------------------------------------------------------------------------
## Be patient
county_dorling <- cartogram_dorling(x = counties_sf,
    weight = "prop_pop", 
    k = 0.2, itermax = 100)

out_black <- county_dorling |>
  filter(!str_detect(name, "Alaska|Hawaii|Puerto|Guam")) |>
  ggplot(aes(fill = black_disc)) +
  geom_sf(color = "grey30", size = 0.1) +
  coord_sf(crs = 2163, datum = NA) +
  scale_fill_discrete_sequential(palette = "YlOrBr", 
                                 na.translate=FALSE) +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1)) + 
  labs(
     subtitle = "Bubble size corresponds to County Population",
     caption = "Graph: @kjhealy. Source: Census Bureau / American Community Survey",
       fill = "Percent Black by County") + 
  theme(legend.position = "top", 
        legend.spacing.x = unit(0, "cm"),
        legend.title = element_text(size = rel(1.5), face = "bold"), 
        legend.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(1.4), hjust = 0.15))

# ggsave("figures/dorling-bl.pdf", out_black, height = 10, width = 12)



## -----------------------------------------------------------------------------
#| echo: FALSE
#| warning: FALSE
#| message: FALSE
out_hispanic <- county_dorling |>
  filter(!str_detect(name, "Alaska|Hawaii|Puerto|Guam")) |>
  ggplot(aes(fill = hisp_disc)) +
  geom_sf(color = "grey30", size = 0.1) +
  coord_sf(crs = 2163, datum = NA) +
  scale_fill_discrete_sequential(palette = "SunsetDark", na.translate=FALSE) +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1, 
                             )) + 
  labs(fill = "Percent Hispanic by County") + 
  theme(legend.position = "top", 
        legend.spacing.x = unit(0, "cm"),
        legend.title = element_text(size = rel(1.5), face = "bold"), 
        legend.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(1.4), hjust = 0.15))

# ggsave("figures/dorling-hs.pdf", out_hispanic, height = 10, width = 12)


out_white <- county_dorling |>
  filter(!str_detect(name, "Alaska|Hawaii|Puerto|Guam")) |>
  ggplot(aes(fill = nhwhite_disc)) +
  geom_sf(color = "grey30", size = 0.1) +
  coord_sf(crs = 2163, datum = NA) +
  scale_fill_discrete_sequential(palette = "BluYl", na.translate=FALSE) +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1, 
                             )) + 
  labs(fill = "Percent Non-Hispanic White by County") + 
  theme(legend.position = "top", 
        legend.spacing.x = unit(0, "cm"),
        legend.title = element_text(size = rel(1.5), face = "bold"), 
        legend.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(1.4), hjust = 0.15))

# ggsave("figures/dorling-wh.pdf", out_white, height = 10, width = 12)

out_asian <- county_dorling |>
  filter(!str_detect(name, "Alaska|Hawaii|Puerto|Guam")) |>
  ggplot(aes(fill = asian_disc)) +
  geom_sf(color = "grey30", size = 0.1) +
  coord_sf(crs = 2163, datum = NA) +
  scale_fill_discrete_sequential(palette = "Purple-Ora", na.translate=FALSE) +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             nrow = 1, 
                             )) + 
  labs(fill = "Percent Asian by County") + 
  theme(legend.position = "top", 
        legend.spacing.x = unit(0, "cm"),
        legend.title = element_text(size = rel(1.5), face = "bold"), 
        legend.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(1.4), hjust = 0.15))

# ggsave("figures/dorling-asian.pdf", out_asian, height = 10, width = 12)



## -----------------------------------------------------------------------------
#| fig.width: 10
#| fig.height: 8
print(out_black)


## -----------------------------------------------------------------------------
#| fig.width: 10
#| fig.height: 8
print(out_white)


## -----------------------------------------------------------------------------
#| fig.width: 10
#| fig.height: 8
print(out_hispanic)


## -----------------------------------------------------------------------------
#| fig.width: 10
#| fig.height: 8
print(out_asian)


## -----------------------------------------------------------------------------
#| echo: FALSE
#| message: FALSE

## Clean up. These files end up in different places conditional on 
## whether we're running this standalone or in a targets pipeline
if(fs::file_exists(here::here("slides", "fbnyc2.Rdata"))) fs::file_delete(here::here("slides", "fbnyc2.Rdata"))
if(fs::file_exists(here::here("slides", "fb_map.Rdata"))) fs::file_delete(here::here("slides", "fb_map.Rdata"))
if(fs::file_exists(here::here("slides", "nyc_fb.Rdata"))) fs::file_delete(here::here("slides", "nyc_fb.Rdata"))

if(fs::file_exists(here::here("fbnyc2.Rdata"))) fs::file_delete(here::here("fbnyc2.Rdata"))
if(fs::file_exists(here::here("fb_map.Rdata"))) fs::file_delete(here::here("fb_map.Rdata"))
if(fs::file_exists(here::here("nyc_fb.Rdata"))) fs::file_delete(here::here("nyc_fb.Rdata"))




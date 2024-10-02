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
library(here)       # manage file paths
library(socviz)     # data and some useful functions
library(tidyverse)  # your friend and mine
library(maps)       # Some basic maps
library(ggforce)    # ggplot extensions


## -----------------------------------------------------------------------------
#| label: "07-make-maps-6"
## This is from the map library
# library(maps)

us_states <- map_data("state")

dim(us_states)

## Making it a tibble prevents crashes 
## in the slide rendering later on
us_states <- as_tibble(us_states)

us_states



## -----------------------------------------------------------------------------
#| label: "07-make-maps-7"
us_states



## -----------------------------------------------------------------------------
#| label: "codefig-poly1"
#| message: FALSE
#| fig.width: 6
#| fig.height: 4.5
#| output-location: column

us_states |>
  ggplot(mapping = aes(x = long, 
                       y = lat, 
                       group = group)) +
  geom_polygon(fill = "white", 
               color = "black") +
  labs(title = "This looks horrible")



## -----------------------------------------------------------------------------
#| label: "codefig-poly2"
#| message: FALSE
#| fig.width: 6
#| fig.height: 4.5
#| output-location: column

us_states |>
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = region,#<<
                       group = group)) +
  geom_polygon(color = "black") + 
  guides(fill = "none") + #<<
  labs(title = "Still looks horrible", 
       caption = "Set fill = none 
         to stop ggplot from 
         producing a key
         with 50 entries")



## -----------------------------------------------------------------------------
#| label: "reveal-coord"
#| include: FALSE
us_states <- as_tibble(map_data("state"))

us_states |>
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = region,
                       group = group)) +
  geom_polygon(color = "black") + 
  guides(fill = "none") +
  coord_map(projection = "albers", 
            lat0 = 39, 
            lat1 = 45) 


## -----------------------------------------------------------------------------
#| label: "07-make-maps-10"
#| echo: FALSE
#| fig.height: 8
#| fig.width: 15
us_states |>
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = region,#<<
                       group = group)) +
  geom_polygon(color = "black") + 
  coord_map(projection = "albers", #<
            lat0 = 39,  #<
            lat1 = 45) + #<
  guides(fill = "none")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-11"
us_states


## -----------------------------------------------------------------------------
#| label: "07-make-maps-12"
election


## -----------------------------------------------------------------------------
#| label: "07-make-maps-13"
election <- election |>
  mutate(region = tolower(state)) |> 
  relocate(region)

election


## -----------------------------------------------------------------------------
#| label: "07-make-maps-14"
us_states


## -----------------------------------------------------------------------------
#| label: "07-make-maps-15"
election


## -----------------------------------------------------------------------------
#| label: "07-make-maps-16"
us_states_elec <- left_join(us_states, election, by = "region")

us_states_elec


## -----------------------------------------------------------------------------
#| label: "codefig-choroparty"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

us_states_elec |>
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = party,#<<
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  guides(fill = "none")




## -----------------------------------------------------------------------------
#| label: "07-make-maps-18"
theme_map <- function(base_size=9, base_family="") {
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
              legend.position = c(0,0)
              )
}


## -----------------------------------------------------------------------------
#| label: "codefig-choropartytheme"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

us_states_elec |> 
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = party,#<<
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  theme_map()


## -----------------------------------------------------------------------------
#| label: "codefig-choropartycolors"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

## Hex color codes for Democratic Blue and Republican Red
party_colors <- c("#2E74C0", "#CB454A")


us_states_elec |> 
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = party,#<<
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  scale_fill_manual(values = party_colors) + 
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  theme_map()



## -----------------------------------------------------------------------------
#| label: "codefig-gradient1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

us_states_elec |> 
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = pct_trump,#<<
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  labs(title = "Trump vote", 
       fill = "Percent") +  
  theme_map()



## -----------------------------------------------------------------------------
#| label: "codefig-gradient2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

us_states_elec |> 
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = pct_trump,
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  scale_fill_gradient(low = "white",  #<<
                      high = "#CB454A") + #<<
        labs(title = "Trump vote") +
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  labs(title = "Trump vote", 
       fill = "Percent") +  
  theme_map()




## -----------------------------------------------------------------------------
#| label: "codefig-diverging1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

us_states_elec |> 
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = d_points,#<<
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  scale_fill_gradient2() + #<<
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  labs(title = "Winning Margins", 
       fill = "Percent") +  
  theme_map()




## -----------------------------------------------------------------------------
#| label: "codefig-purpleamerica"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

us_states_elec |> 
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = d_points,#<<
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  scale_fill_gradient2(low = "red",#<<
                mid = scales::muted("purple"),#<<
                high = "blue",#<<
                breaks = c(-25, 0, 25, #<<
                        50, 75)) + #<<
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  labs(title = "Winning Margins", 
       fill = "Percent") +  
  theme_map()





## -----------------------------------------------------------------------------
#| label: "07-make-maps-25"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 7
us_states_elec |> 
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = d_points,#<<
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  scale_fill_gradient2(low = "red",#<<
                mid = scales::muted("purple"),#<<
                high = "blue",#<<
                breaks = c(-25, 0, 25, #<<
                        50, 75)) + #<<
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  labs(title = "Winning Margins", 
       fill = "Percent") +  
  theme_map()



## -----------------------------------------------------------------------------
#| label: "codefig-purpleamerica2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

us_states_elec |> 
  filter(region %nin% "district of columbia") |> #<<
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = d_points,
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  scale_fill_gradient2(low = "red",
                mid = scales::muted("purple"),
                high = "blue") +
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  labs(title = "Winning Margins", 
       fill = "Percent") +  
  theme_map()



## -----------------------------------------------------------------------------
#| label: "07-make-maps-27"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 7
us_states_elec |> 
  filter(region %nin% "district of columbia") |> #<<
  ggplot(mapping = aes(x = long, 
                       y = lat,
                       fill = d_points,
                       group = group)) + 
  geom_polygon(color = "gray90", 
               size = 0.1) +
  scale_fill_gradient2(low = "red",
                mid = scales::muted("purple"),
                high = "blue") +
  coord_map(projection = "albers", 
            lat0 = 39, lat1 = 45) +
  labs(title = "Winning Margins", 
       fill = "Percent") +  
  theme_map()



## -----------------------------------------------------------------------------
#| label: "07-make-maps-28"
county_map <- as_tibble(county_map)
county_map


## -----------------------------------------------------------------------------
#| label: "07-make-maps-29"
county_data <- as_tibble(county_data)
county_data


## -----------------------------------------------------------------------------
#| label: "07-make-maps-30"
county_data  |> 
    select(id, name, state, pop_dens, pct_black) |>
    sample_n(10)


## -----------------------------------------------------------------------------
#| label: "07-make-maps-31"
county_full <- as_tibble(left_join(county_map, county_data, by = "id"))

county_full


## -----------------------------------------------------------------------------
#| label: "reveal-countypop"
#| include: FALSE
county_full <- as_tibble(left_join(county_map, county_data, by = "id"))


county_full |> 
  ggplot(mapping = aes(x = long, y = lat,
                          fill = pop_dens, 
                          group = group)) + 
  geom_polygon(color = "gray70", 
               size = 0.1) + 
  coord_fixed() + 
  scale_fill_brewer(palette="Blues",
                    labels = c("0-10", "10-50", "50-100",
                     "100-500", "500-1,000",
                      "1,000-5,000", ">5,000")) + 
  labs(fill = "Population per\nsquare mile") + 
  kjhslides::kjh_theme_map() + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-32"
#| echo: FALSE
#| fig.height: 8
#| fig.width: 12
county_full |> 
  ggplot(mapping = aes(x = long, y = lat,
                          fill = pop_dens, 
                          group = group)) + 
  geom_polygon(color = "gray70", 
               size = 0.1) + 
  coord_fixed() + 
  scale_fill_brewer(palette="Blues",
                    labels = c("0-10", "10-50", "50-100",
                     "100-500", "500-1,000",
                      "1,000-5,000", ">5,000")) + 
  labs(fill = "Population per\nsquare mile") + 
  theme_map() + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
#| label: "reveal-choroblack"
#| include: FALSE
county_full <- as_tibble(left_join(county_map, county_data, by = "id"))

county_full |> 
  ggplot(mapping = aes(x = long, y = lat,
                       fill = pct_black,  
                       group = group)) + 
  geom_polygon(color = "gray70", 
               size = 0.1) + 
  coord_fixed() + 
  scale_fill_brewer(palette="Greens",
                    labels = c("0-2%", "2-5%", "5-10%",
                               "10-15%", "15-25%",
                               "25-50%", ">50%")) + 
  labs(fill = "US Population, percent Black") + 
  kjhslides::kjh_theme_map() + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-33"
#| echo: FALSE
#| fig.height: 8
#| fig.width: 12
county_full |> 
  ggplot(mapping = aes(x = long, y = lat,
                       fill = pct_black, #<<
                       group = group)) + 
  geom_polygon(color = "gray70", 
               size = 0.1) + 
  coord_fixed() + 
  scale_fill_brewer(palette="Greens",
                    labels = c("0-2%", "2-5%", "5-10%",
                               "10-15%", "15-25%",
                               "25-50%", ">50%")) + 
  labs(fill = "US Population, percent Black") + 
  theme_map() + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-34"
orange_pal <- RColorBrewer::brewer.pal(n = 6, 
                                       name = "Oranges")
orange_pal


## -----------------------------------------------------------------------------
#| label: "07-make-maps-35"
#| echo: FALSE
#| fig.height: 0.5
#| fig.width: 5
par(mar = c(0, 0, 0, 0))
colorspace::swatchplot("Orange Palette" = orange_pal, nrow = 1)


## -----------------------------------------------------------------------------
#| label: "07-make-maps-36"
# Just reverse it
orange_rev <- rev(orange_pal)
orange_rev


## -----------------------------------------------------------------------------
#| label: "07-make-maps-37"
#| echo: FALSE
#| fig.height: 0.5
#| fig.width: 5
par(mar = c(0, 0, 0, 0))
colorspace::swatchplot("Reverse Orange" = orange_rev, nrow = 1)


## -----------------------------------------------------------------------------
#| label: "07-make-maps-38"
p_g1 <- county_full |> 
  ggplot(mapping = aes(x = long, y = lat,
                       fill = su_gun6, #<<
                       group = group)) + 
  geom_polygon(color = "gray70", 
               size = 0.1) + 
  coord_fixed() + 
  scale_fill_manual(values = orange_pal) + #<<
  labs(title = "Gun-Related Suicides, 1999-2015",
       fill = "Rate per 100,000 pop.") + 
  theme_map() + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-39"
#| echo: FALSE
#| fig.height: 8
#| fig.width: 12
p_g1 


## -----------------------------------------------------------------------------
#| label: "07-make-maps-40"
p_g2 <- county_full |> 
  ggplot(mapping = aes(x = long, y = lat,
                       fill = pop_dens6, #<<
                       group = group)) + 
  geom_polygon(color = "gray70", 
               size = 0.1) + 
  coord_fixed() + 
  scale_fill_manual(values = orange_rev) + #<<
  labs(title = "Reverse-coded Population Density",
       fill = "Persons per square mile") + 
  theme_map() + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-41"
#| echo: FALSE
#| fig.height: 8
#| fig.width: 12
p_g2


## -----------------------------------------------------------------------------
#| label: "07-make-maps-42"
#| echo: FALSE
#| fig.height: 6
#| fig.width: 8
p_g1


## -----------------------------------------------------------------------------
#| label: "07-make-maps-43"
#| echo: FALSE
#| fig.height: 6
#| fig.width: 8
p_g2


## -----------------------------------------------------------------------------
#| label: "07-make-maps-44"
opiates

opiates$state <- tolower(opiates$state)
us_states$state <- us_states$region
opiates_map <- left_join(us_states, opiates, by = "state")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-45"
p_out <- opiates_map |> 
  ggplot(mapping = aes(x = long, y = lat,
                       group = group,
                       fill = cut_interval(adjusted, n = 5))) + 
  geom_polygon(color = "lightblue", size = 0.2) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_brewer(type = "seq", palette = "Oranges") +
  kjhslides::kjh_theme_map() + 
  facet_wrap(~ year, ncol = 4) +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Death rate per 100,000 population",
       title = "Opiate-Related Deaths by State, 1999-2014")


## -----------------------------------------------------------------------------
#| label: "07-make-maps-46"
#| echo: FALSE
#| fig.height: 8
#| fig.width: 16
p_out


## -----------------------------------------------------------------------------
#| label: "reveal-maxstates"
#| include: FALSE
## Put this in an object called `st_top`
opiates |> 
  filter(year == max(year), 
         abbr != "DC") |> 
  group_by(region) |> 
  slice_max(order_by = adjusted, 
            n = 2)



## -----------------------------------------------------------------------------
#| label: "reveal-opiateline"
#| include: FALSE
st_top <- opiates |>  filter(year == max(year), abbr != "DC") |> 
  group_by(region) |> 
  slice_max(order_by = adjusted, n = 2)
 
opiates |> 
  ggplot(aes(x = year, 
             y = adjusted)) +
  geom_line(aes(group = state),
            color = "gray50") + 
  geom_smooth(aes(group = region),
              se = FALSE) + 
  ggrepel::geom_text_repel(
    data = st_top,
    mapping = aes(x = year, 
                  y = adjusted, 
                  label = abbr), 
    size = 3, 
    segment.color = NA, 
    nudge_x = 0.5) +
  coord_cartesian(c(min(opiates$year), 
                    max(opiates$year) + 1)) + 
  labs(x = NULL, 
       y = "Rate per 100,000 population",
       title = "State-Level Opiate Death 
          Rates by Region, 1999-2014") +
  facet_wrap(~ reorder(region, adjusted, 
                       na.rm = TRUE), 
             nrow  = 1)
  



## -----------------------------------------------------------------------------
#| label: "07-make-maps-48"
#| echo: FALSE
#| fig.height: 6
#| fig.width: 15
p_out <- opiates |> 
  ggplot(aes(x = year, 
             y = adjusted)) +
  geom_line(aes(group = state),
            color = "gray50") + 
  geom_smooth(aes(group = region),
              se = FALSE) + 
  ggrepel::geom_text_repel(
    data = st_top,
    mapping = aes(x = year, 
                  y = adjusted, 
                  label = abbr), 
    size = 3, 
    segment.color = NA, 
    nudge_x = 0.5) +
  coord_cartesian(c(min(opiates$year), 
                    max(opiates$year) + 1)) + 
  labs(x = NULL, 
       y = "Rate per 100,000 population",
       title = "State-Level Opiate Death 
          Rates by Region, 1999-2014") +
  facet_wrap(~ reorder(region, adjusted, 
                       na.rm = TRUE), 
             nrow  = 1)
  
p_out


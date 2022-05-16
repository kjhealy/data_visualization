## ----note, include=FALSE---------------------------------------------------------------------------------------------------------------------------
## NB: By default the  template will create a new subdirectory with its files inside.


## ----packages, include=FALSE-----------------------------------------------------------------------------------------------------------------------
library(flipbookr)
library(here)
library(tidyverse)
library(kjhslides)


## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------
## Configure the slides

kjh_register_tenso()    # Default fonts. Comment out if you don't have Tenso and Berkeley fonts.
kjh_set_knitr_opts()    
kjh_set_slide_theme()   # ggplot theme to go with slides. Set tenso = FALSE if necessary.
kjh_set_xaringan_opts()



## ----07-make-maps-1, message = FALSE---------------------------------------------------------------------------------------------------------------
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine
library(maps)      # Some basic maps
library(sf)        # Simple Features Geometries and geom_sf()
library(ggforce)   # Useful enhancements to ggplot


## ----07-make-maps-2--------------------------------------------------------------------------------------------------------------------------------
## Hex color codes for Democratic Blue and Republican Red
party_colors <- c("#2E74C0", "#CB454A")


## ----07-make-maps-3--------------------------------------------------------------------------------------------------------------------------------
election |> 
  select(state, total_vote, r_points, pct_trump, party, census)


## ----reveal-elecfacet1, include = FALSE------------------------------------------------------------------------------------------------------------

election |> 
  filter(st %nin% "DC") |> 
  ggplot(mapping = aes(x = r_points,
                       y = reorder(state, r_points),
                       color = party)) + 
  geom_vline(xintercept = 0, 
             color = "gray30") +
  geom_point(size = 2) + 
  scale_color_manual(values = party_colors) + 
  scale_x_continuous(breaks = c(-30, -20, -10, 0, 
                                10, 20, 30, 40),
                     labels = c("30\n (Clinton)", 
                                "20", "10", "0",
                                "10", "20", "30", 
                                "40\n(Trump)")) + 
  facet_wrap(~ census, ncol=2, 
             scales="free_y") +
  guides(color = "none") + 
  labs(x = "Point Margin", y = NULL) +
    theme(axis.text=element_text(size=8))




## ----07-make-maps-4, echo = FALSE, fig.height=6, fig.width=8---------------------------------------------------------------------------------------
p_out <- election |> 
  filter(st %nin% "DC") |> 
  ggplot(mapping = aes(x = r_points,
                       y = reorder(state, r_points),
                       color = party)) + 
  geom_vline(xintercept = 0, 
             color = "gray30") +
  geom_point(size = 2) + 
  scale_color_manual(values = party_colors) + 
  scale_x_continuous(breaks = c(-30, -20, -10, 0, 
                                10, 20, 30, 40),
                     labels = c("30\n (Clinton)", 
                                "20", "10", "0",
                                "10", "20", "30", 
                                "40\n(Trump)")) + 
  facet_wrap(~ census, ncol=2, 
             scales="free_y") +
  guides(color = "none") + 
  labs(x = "Point Margin", y = NULL) +
    theme(axis.text=element_text(size=8))

p_out


## ----codefig-ggforce, message=FALSE, fig.show="hide", fig.width=2.5, fig.height=5.65---------------------------------------------------------------

p_out <- election |> 
  filter(st %nin% "DC") |> 
  ggplot(mapping = aes(x = r_points,
                       y = reorder(state, r_points),
                       color = party)) + 
  geom_vline(xintercept = 0, 
             color = "gray30") +
  geom_point(size = 2) + 
  scale_color_manual(values = party_colors) + 
  scale_x_continuous(breaks = c(-30, -20, -10, 0, 
                                10, 20, 30, 40),
                     labels = c("30\n (Clinton)", 
                                "20", "10", "0",
                                "10", "20", "30", 
                                "40\n(Trump)")) + 
  facet_col(~ census, #<<
            scales="free_y", #<<
            space = "free") + #<<
  guides(color = "none") + 
  labs(x = "Point Margin", y = NULL) +
    theme(axis.text=element_text(size=6), 
          strip.text = element_text(size = rel(0.6)))

p_out



## ----07-make-maps-5, echo=FALSE--------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-ggforce", "png"))


## ----07-make-maps-6--------------------------------------------------------------------------------------------------------------------------------
## This is from the map library
# library(maps)

us_states <- map_data("state")

dim(us_states)

## Making it a tibble prevents crashes 
## in the slide rendering later on
us_states <- as_tibble(us_states)

us_states



## ----07-make-maps-7--------------------------------------------------------------------------------------------------------------------------------
us_states



## ----codefig-poly1, message=FALSE, fig.show="hide", fig.width=6, fig.height=4.5--------------------------------------------------------------------
  
us_states |> 
  ggplot(mapping = aes(x = long, 
                       y = lat, 
                       group = group)) +
  geom_polygon(fill = "white", 
               color = "black") +
  labs(title = "This looks horrible")



## ----07-make-maps-8, echo=FALSE--------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-poly1", "png"))


## ----codefig-poly2, message=FALSE, fig.show="hide", fig.width=6, fig.height=4.5--------------------------------------------------------------------

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



## ----07-make-maps-9, echo=FALSE--------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-poly2", "png"))


## ----reveal-coord, include = FALSE-----------------------------------------------------------------------------------------------------------------

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

## A coordinate transformation!



## ----07-make-maps-10, echo = FALSE, fig.height=8, fig.width=15-------------------------------------------------------------------------------------
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


## ----07-make-maps-11-------------------------------------------------------------------------------------------------------------------------------
us_states


## ----07-make-maps-12-------------------------------------------------------------------------------------------------------------------------------
election


## ----07-make-maps-13-------------------------------------------------------------------------------------------------------------------------------
election <- election |> 
  mutate(region = tolower(state)) |> 
  relocate(region)

election


## ----07-make-maps-14-------------------------------------------------------------------------------------------------------------------------------
us_states


## ----07-make-maps-15-------------------------------------------------------------------------------------------------------------------------------
election


## ----07-make-maps-16-------------------------------------------------------------------------------------------------------------------------------
us_states_elec <- left_join(us_states, election, by = "region")

us_states_elec


## ----codefig-choroparty, message=FALSE, fig.show="hide", fig.width=4.8, fig.height=4.5-------------------------------------------------------------

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




## ----07-make-maps-17, echo=FALSE-------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-choroparty", "png"))


## ----07-make-maps-18-------------------------------------------------------------------------------------------------------------------------------
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


## ----codefig-choropartytheme, message=FALSE, fig.show="hide", fig.width=4.8, fig.height=4.5--------------------------------------------------------

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




## ----07-make-maps-19, echo=FALSE-------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-choropartytheme", "png"))


## ----codefig-choropartycolors, message=FALSE, fig.show="hide", fig.width=4.8, fig.height=4.5-------------------------------------------------------
  
## recall
party_colors

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



## ----07-make-maps-20, echo=FALSE-------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-choropartycolors", "png"))


## ----codefig-gradient1, message=FALSE, fig.show="hide", fig.width=4.8, fig.height=4.5--------------------------------------------------------------

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



## ----07-make-maps-21, echo=FALSE-------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-gradient1", "png"))


## ----codefig-gradient2, message=FALSE, fig.show="hide", fig.width=4.8, fig.height=4.5--------------------------------------------------------------

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




## ----07-make-maps-22, echo=FALSE-------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-gradient2", "png"))


## ----codefig-diverging1, message=FALSE, fig.show="hide", fig.width=4.8, fig.height=4.5-------------------------------------------------------------

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




## ----07-make-maps-23, echo=FALSE-------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-diverging1", "png"))


## ----codefig-purpleamerica, message=FALSE, fig.show="hide", fig.width=4.8, fig.height=4.5----------------------------------------------------------

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





## ----07-make-maps-24, echo=FALSE-------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-purpleamerica", "png"))


## ----07-make-maps-25, echo = FALSE, fig.width=12, fig.height=7-------------------------------------------------------------------------------------
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



## ----codefig-purpleamerica2, message=FALSE, fig.show="hide", fig.width=4.8, fig.height=4.5---------------------------------------------------------

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





## ----07-make-maps-26, echo=FALSE-------------------------------------------------------------------------------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-purpleamerica2", "png"))


## ----07-make-maps-27, echo = FALSE, fig.width=12, fig.height=7-------------------------------------------------------------------------------------
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



## ----07-make-maps-28-------------------------------------------------------------------------------------------------------------------------------
county_map <- as_tibble(county_map)
county_map


## ----07-make-maps-29-------------------------------------------------------------------------------------------------------------------------------
county_data <- as_tibble(county_data)
county_data


## ----07-make-maps-30-------------------------------------------------------------------------------------------------------------------------------
county_data  |> 
    select(id, name, state, pop_dens, pct_black) %>%
    sample_n(10)


## ----07-make-maps-31-------------------------------------------------------------------------------------------------------------------------------
county_full <- left_join(county_map, county_data, by = "id")

county_full


## ----reveal-countypop, include = FALSE-------------------------------------------------------------------------------------------------------------
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


## ----07-make-maps-32, echo = FALSE, fig.height=8, fig.width=12-------------------------------------------------------------------------------------
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


## ----reveal-choroblack, include = FALSE------------------------------------------------------------------------------------------------------------
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
  theme_map() + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")


## ----07-make-maps-33, echo = FALSE, fig.height=8, fig.width=12-------------------------------------------------------------------------------------
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


## ----07-make-maps-34-------------------------------------------------------------------------------------------------------------------------------
orange_pal <- RColorBrewer::brewer.pal(n = 6, 
                                       name = "Oranges")
orange_pal


## ----07-make-maps-35, echo = FALSE, fig.height=0.5, fig.width=5------------------------------------------------------------------------------------
par(mar = c(0, 0, 0, 0))
colorspace::swatchplot("Orange Palette" = orange_pal, nrow = 1)


## ----07-make-maps-36-------------------------------------------------------------------------------------------------------------------------------
# Just reverse it
orange_rev <- rev(orange_pal)
orange_rev


## ----07-make-maps-37, echo = FALSE, fig.height=0.5, fig.width=5------------------------------------------------------------------------------------
par(mar = c(0, 0, 0, 0))
colorspace::swatchplot("Reverse Orange" = orange_rev, nrow = 1)


## ----07-make-maps-38-------------------------------------------------------------------------------------------------------------------------------
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


## ----07-make-maps-39, echo = FALSE, fig.height=8, fig.width=12-------------------------------------------------------------------------------------
p_g1 


## ----07-make-maps-40-------------------------------------------------------------------------------------------------------------------------------
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


## ----07-make-maps-41, echo = FALSE, fig.height=8, fig.width=12-------------------------------------------------------------------------------------
p_g2


## ----07-make-maps-42, echo = FALSE,, fig.height=6, fig.width=8-------------------------------------------------------------------------------------
p_g1


## ----07-make-maps-43, echo = FALSE, fig.height=6, fig.width=8--------------------------------------------------------------------------------------
p_g2


## ----07-make-maps-44-------------------------------------------------------------------------------------------------------------------------------
opiates

opiates$state <- tolower(opiates$state)
us_states$state <- us_states$region
opiates_map <- left_join(us_states, opiates, by = "state")


## ----07-make-maps-45-------------------------------------------------------------------------------------------------------------------------------
p_out <- opiates_map |> 
  ggplot(mapping = aes(x = long, y = lat,
                       group = group,
                       fill = cut_interval(adjusted, n = 5))) + 
  geom_polygon(color = "lightblue", size = 0.2) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_brewer(type = "seq", palette = "Oranges") +
  theme_map() + 
  facet_wrap(~ year, ncol = 4) +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Death rate per 100,000 population",
       title = "Opiate-Related Deaths by State, 1999-2014")


## ----07-make-maps-46, echo = FALSE, fig.height=8, fig.width=16-------------------------------------------------------------------------------------
p_out


## ----reveal-maxstates, include = FALSE-------------------------------------------------------------------------------------------------------------
## Put this in an object called `st_top`
opiates |> 
  filter(year == max(year), 
         abbr != "DC") |> 
  group_by(region) |> 
  slice_max(order_by = adjusted, 
            n = 2)



## ----07-make-maps-47, echo = FALSE-----------------------------------------------------------------------------------------------------------------
st_top <- opiates |> 
  filter(year == max(year), 
         abbr != "DC") |> 
  group_by(region) |> 
  slice_max(order_by = adjusted, 
            n = 2)


## ----reveal-opiateline, include = FALSE------------------------------------------------------------------------------------------------------------
 
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
  



## ----07-make-maps-48, echo = FALSE, fig.height = 6, fig.width = 15---------------------------------------------------------------------------------
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


## ----07-make-maps-49, echo = FALSE-----------------------------------------------------------------------------------------------------------------
detach(package:sf)


## ----07-make-maps-50, message=TRUE-----------------------------------------------------------------------------------------------------------------
library(sf)


## ----07-make-maps-51-------------------------------------------------------------------------------------------------------------------------------
library(nycdogs)
nyc_license


## ----07-make-maps-52-------------------------------------------------------------------------------------------------------------------------------
nyc_zips


## ----07-make-maps-53-------------------------------------------------------------------------------------------------------------------------------
nyc_zips |> 
  select(objectid:borough)


## ----reveal-nycfrench, include = FALSE-------------------------------------------------------------------------------------------------------------
nyc_license  |> 
  filter(extract_year == 2018) |> 
  group_by(breed_rc, zip_code) |> 
  tally() |> 
  mutate(freq = n / sum(n)) |> 
  filter(breed_rc == "French Bulldog") ->
  nyc_fb



## ----07-make-maps-54-------------------------------------------------------------------------------------------------------------------------------
nyc_zips |> select(objectid:st_fips)


## ----07-make-maps-55-------------------------------------------------------------------------------------------------------------------------------
nyc_fb |> select(breed_rc:n)


## ----07-make-maps-56-------------------------------------------------------------------------------------------------------------------------------
fb_map <- left_join(nyc_zips, nyc_fb, by = "zip_code")


## ----07-make-maps-57-------------------------------------------------------------------------------------------------------------------------------
fb_map |> select(zip_code, po_name, borough, breed_rc:freq, geometry)


## ----07-make-maps-58-------------------------------------------------------------------------------------------------------------------------------
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



## ----reveal-bulldogmap, include = FALSE------------------------------------------------------------------------------------------------------------
fb_map %>% 
    ggplot(mapping = aes(fill = freq)) +
    geom_sf(color = "gray80", size = 0.1) + #<<
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
    theme_nymap() + 
   guides(fill = 
           guide_legend(title.position = "top", 
                label.position = "bottom",
                keywidth = 1, 
                nrow = 1))  
 


## ----reveal-bulldogmap2, include = FALSE-----------------------------------------------------------------------------------------------------------
library(colorspace)

fb_map |> 
  ggplot(mapping = aes(fill = freq)) +
  geom_sf(color = "gray80", size = 0.1) +
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
  theme_nymap() + 
  guides(fill = 
           guide_legend(title.position = "top", 
                label.position = "bottom",
                keywidth = 1, 
                nrow = 1))  



## ----07-make-maps-59, echo = FALSE, fig.width=12, fig.height=8-------------------------------------------------------------------------------------
fb_map |> 
  ggplot(mapping = aes(fill = freq)) +
  geom_sf(color = "gray80", size = 0.1) +
  scale_fill_continuous_sequential(
    palette = "Oranges",
    labels = scales::label_percent()) +
  labs(fill = "Percent of All French Bulldogs") +
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
  theme_nymap() + 
  guides(fill = 
           guide_legend(title.position = "top", 
                label.position = "bottom",
                keywidth = 1, 
                nrow = 1))  



## ----reveal-bulldogzeros, include = FALSE----------------------------------------------------------------------------------------------------------

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



## ----07-make-maps-60-------------------------------------------------------------------------------------------------------------------------------
fb_map2 |> select(zip_code, po_name, borough, breed_rc:freq, geometry)


## ----reveal-bulldogfb2, include = FALSE------------------------------------------------------------------------------------------------------------
fb_map2 |> 
  ggplot(mapping = aes(fill = freq)) +
  geom_sf(color = "gray80", size = 0.1) +
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
  theme_nymap() + 
  guides(fill = 
           guide_legend(title.position = "top", 
                        label.position = "bottom",
                        keywidth = 1, 
                        nrow = 1))  



## ----07-make-maps-61, echo = FALSE, fig.width=12, fig.height=8-------------------------------------------------------------------------------------
fb_map2 |> 
  ggplot(mapping = aes(fill = freq)) +
  geom_sf(color = "gray80", size = 0.1) +
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
  theme_nymap() + 
  guides(fill = 
           guide_legend(title.position = "top", 
                        label.position = "bottom",
                        keywidth = 1, 
                        nrow = 1))  



kjh_register_tenso()    # Default fonts. Comment out if you don't have Tenso and Berkeley fonts.
kjh_set_knitr_opts()    
kjh_set_slide_theme()   # ggplot theme to go with slides. Set tenso = FALSE if necessary.




## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-1"
#| message: FALSE
library(here)       # manage file paths
library(tidyverse)  # your friend and mine
library(socviz)     # data and some useful functions
library(ggrepel)    # Text and labels
library(colorspace) # luminance-balanced palettes
library(scales)      # scale adjustments and enhancements
library(ggforce)    # useful enhancements to ggplot


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-2"
asasec <- as_tibble(asasec)
asasec


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-3"
#| echo: FALSE
#| fig.height: 6
#| fig.width: 10
asasec |> 
  filter(Year == 2014) |> 
  ggplot(mapping = aes(x = Members, 
                       y = Revenues, 
                       label = Sname)) + 
  geom_smooth() + 
  geom_point()



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-4"
#| echo: FALSE
#| fig.height: 6
#| fig.width: 10
asasec |> 
  filter(Year == 2014) |> 
  ggplot(mapping = aes(x = Members, 
                       y = Revenues, 
                       label = Sname)) + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "gray60") +
  geom_point(mapping = aes(color = Journal), 
             size = rel(3)) + 
  geom_text_repel(data=subset(asasec,
                    Year == 2014 & 
                    Revenues > 7000),
                    size = rel(5), 
                    mapping = 
                    aes(family = "Tenso Slide")) + 
  scale_y_continuous(labels = 
                       scales::label_dollar()) + 
  labs(x="Membership", y="Revenues",
        color = "Section has own Journal",
        title = "ASA Sections",
        subtitle = "2014 Calendar year.",
        caption = "Source: ASA annual report.") + 
  theme(legend.position = "bottom")
  



## -----------------------------------------------------------------------------
#| label: "reveal-asasteps"
#| include: FALSE
asasec |> 
  filter(Year == 2014) |> 
  ggplot(mapping = aes(x = Members, 
                       y = Revenues, 
                       label = Sname)) + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "gray60") +
  geom_point(mapping = aes(color = Journal), 
             size = rel(3)) + 
  geom_text_repel(data=subset(asasec,
                    Year == 2014 & 
                    Revenues > 7000),
                    size = rel(5), 
                    mapping = 
                    aes(family = "Tenso Slide")) + 
  scale_y_continuous(labels = 
                       scales::label_dollar()) + 
  labs(x="Membership", y="Revenues",
        color = "Section has own Journal",
        title = "ASA Sections",
        subtitle = "2014 Calendar year.",
        caption = "Source: ASA annual report.") + 
  theme(legend.position = "bottom")



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-5"
#| echo: FALSE
kjh_set_classic_theme()


## -----------------------------------------------------------------------------
#| label: "reveal-scalemanual"
#| include: FALSE
organdata |> 
  ggplot(mapping = aes(x = roads,
                       y = donors, 
                       color = world)) + 
  geom_point(size = 4) -> 
  p




## -----------------------------------------------------------------------------
#| eval: FALSE
## p <- organdata |>
##   ggplot(mapping = aes(x = roads,
##                        y = donors,
##                        color = world)) +
##   geom_point(size = 4)


## -----------------------------------------------------------------------------
#| label: "reveal-scalemanual2"
#| include: FALSE
p + 
  scale_color_manual(
  values = c("red", "green", "blue"))



## -----------------------------------------------------------------------------
#| label: "reveal-scalemanual3"
#| include: FALSE
p + 
  scale_color_manual(
  values = c("#1B9E77","#D95F02", "#7570B3"))



## -----------------------------------------------------------------------------
#| label: "reveal-scalemanual4"
#| include: FALSE

colkey <- c("Corporatist" = "pink", 
            "Liberal" = "goldenrod", 
            "SocDem" = "firebrick")
colkey

p + scale_color_manual(
  values = colkey)



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-6"
#| echo: FALSE
opar <- par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mar=c(0,4,0,0))
RColorBrewer::display.brewer.all(type = "qual")
par(opar)


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-7"
#| echo: FALSE
par(mar=c(0,4,0,0))
RColorBrewer::display.brewer.all(type = "seq")


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-8"
#| echo: FALSE
par(mar=c(0,4,0,0))
RColorBrewer::display.brewer.all(type = "div")


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-9"
#| eval: FALSE
#| fig.show: "hold"
#| out.width: "50%"
## p + geom_point(size = 2) +
##   scale_color_brewer(palette = "Set2") +
##   labs(title = "Set2")
## 
## p + geom_point(size = 2) +
##   scale_color_brewer(palette = "Pastel2") +
##   labs(title = "Pastel2")
## 
## p + geom_point(size = 2) +
##   scale_color_brewer(palette = "Dark2") +
##   labs(title = "Dark2")
## 
## p + geom_point(size = 2) +
##   scale_color_brewer(palette = "Accent") +
##   labs(title = "Accent")


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-10"
#| echo: FALSE
p + geom_point(size = 3) + 
  scale_color_brewer(palette = "Set2") +
  labs(title = "Set2")


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-11"
#| echo: FALSE
p + geom_point(size = 3) + 
  scale_color_brewer(palette = "Pastel2") +
  labs(title = "Pastel2")


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-12"
#| echo: FALSE
p + geom_point(size = 3) + 
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Dark2")


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-13"
#| echo: FALSE
p + geom_point(size = 3) + 
  scale_color_brewer(palette = "Accent") +
  labs(title = "Accent")


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-14"
#| fig.height: 8
#| fig.width: 16
#| echo: FALSE
colorspace::hcl_palettes(plot = TRUE)


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-15"
#| echo: FALSE
kjh_set_slide_theme()


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-16"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
# Democratic Blue and Republican Red
party_colors <- c("#2E74C0", "#CB454A")
party_colors <- c("royalblue1", "red2")

ggplot(data = subset(county_data,
                     flipped == "No"),
       mapping = aes(x = pop,
                     y = black/100)) + 
  geom_point(alpha = 0.15, color = "gray30", 
             size = rel(2)) +
  scale_x_log10(labels = label_comma()) + 
  geom_point(data = subset(county_data,
                      flipped == "Yes"),
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16), 
             size = rel(2)) +
  geom_text_repel(data = subset(county_data,
                                flipped == "Yes" &
                                  black  > 25),
                  mapping = aes(x = pop,
                                y = black/100,
                                label = state, 
                                family = "Tenso Slide", 
                                face = "bold"), 
                  size = 3.5) + 
  scale_color_manual(values = party_colors) + 
  scale_y_continuous(labels = label_percent()) +
  labs(color = "County flipped to ... ",
       x = "County Population (log scale)",
       y = "Percent Black Population",
       title = "Flipped counties, 2016",
       caption = "Counties in gray did not flip.")




## -----------------------------------------------------------------------------
#| label: "reveal-fliptrump"
#| include: FALSE

## Brighter Blue and Red
party_colors <- c("royalblue1", "red2")

ggplot(data = subset(county_data,
                     flipped == "No"),
       mapping = aes(x = pop,
                     y = black/100)) + 
  geom_point(alpha = 0.15, color = "gray30", 
             size = rel(2)) +
  scale_x_log10(labels = label_comma()) + 
  geom_point(data = subset(county_data,
                      flipped == "Yes"),
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16), 
             size = rel(2)) +
  geom_text_repel(data = subset(county_data,
              flipped == "Yes" & black  > 25),
                  mapping = aes(x = pop,
                    y = black/100, label = state, 
                    family = "Tenso Slide", 
                    face = "bold"), size = rel(3.5)) + 
  scale_color_manual(values = party_colors) + 
  scale_y_continuous(labels = label_percent()) +
  labs(color = "County flipped to ... ",
       x = "County Population (log scale)",
       y = "Percent Black Population",
       title = "Flipped counties, 2016",
       caption = "Counties in gray did not flip.")
 


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-17"
#| echo: FALSE
county_data <- as_tibble(county_data) |> 
  filter(!is.na(name) & name %nin% as.character(c(1:52)))

pop_min <- min(county_data$pop)
pop_max <- max(county_data$pop)

black_min <- min(county_data$black/100)
black_max <- max(county_data$black/100 + 0.047)

x_label <- "County Population (log scale)"
y_label <- "Percent Black Population"

x_breaks <- c(1e3, 1e4, 1e5, 1e6, 1e7)
y_breaks <- seq(from = 0, to = 0.8, by = 0.2)
data_point_size <- rel(3)

p_layer_1 <- ggplot(data = county_data,
                    mapping = aes(x = pop, y = black/100)) + 
  geom_point(color = "gray20", 
             alpha = 0.25, 
             size = data_point_size) + 
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) + 
  scale_y_continuous(breaks = y_breaks, 
                     labels = label_percent()) +
  expand_limits(x = pop_max, 
                y = black_max) +
  labs(x = x_label,
       y = y_label,
       title = "U.S. Counties by Population and Percent Black",
       caption = "")

p_layer_2 <- ggplot(data = subset(county_data,
                     flipped == "No"),
                    mapping = aes(x = pop, y = black/100)) + 
  geom_point(color = "gray20", 
             alpha = 0.25, 
             size = data_point_size) + 
  expand_limits(x = pop_max, 
                y = black_max) +
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) + 
  scale_y_continuous(breaks = y_breaks, 
                     labels = label_percent()) +
  labs(x = x_label,
       y = y_label,
       title = "These counties did not flip in 2016",
       caption = "")


p_layer_3 <- ggplot(data = subset(county_data,
                     flipped == "Yes"),
                    mapping = aes(x = pop, y = black/100)) + 
  geom_point(color = "gray5", 
             alpha = 0.25, 
             size = data_point_size) + 
   geom_point(data = subset(county_data,
                     flipped == "Yes"), 
              color = NA, 
              alpha = 0,
              size = data_point_size) +
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) + 
  scale_y_continuous(breaks = y_breaks, 
                     labels = label_percent()) +
  expand_limits(x = pop_max, 
                y = black_max) +
  labs(x = x_label,
       y = y_label,
       title = "These counties did",
       caption = "")


p_layer_4 <- ggplot(data = subset(county_data,
                     flipped == "No"),
                    mapping = aes(x = pop, y = black/100)) + 
  geom_point(color = "gray30", 
             alpha = 0.25, 
             size = data_point_size) + 
  geom_point(data = subset(county_data,
                      flipped == "Yes"),
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16), 
             size = data_point_size) + 
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) + 
  scale_y_continuous(breaks = y_breaks, 
                     labels = label_percent()) +
  scale_color_manual(values = party_colors) +
  expand_limits(x = pop_max, 
                y = black_max) +
  labs(x = x_label,
       y = y_label,
       title = "Counties that flipped shown by party color",
       color = "Flipped to",
       caption = "Counties in gray did not flip.")

p_layer_5 <- ggplot(data = subset(county_data,
                     flipped == "No"),
                    mapping = aes(x = pop, y = black/100)) + 
  geom_point(color = "gray30", 
             alpha = 0.25, 
             size = data_point_size) + 
  geom_point(data = subset(county_data,
                      flipped == "Yes"),
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16), 
             size = rel(2)) + 
  geom_label_repel(data = subset(county_data,
              flipped == "Yes" & black  > 25),
                  mapping = aes(x = pop,
                    y = black/100, label = state, 
                    family = "Tenso Slide", 
                    face = "bold"), size = rel(3.5)) + 
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) + 
  scale_y_continuous(breaks = y_breaks, 
                     labels = label_percent()) +
  scale_color_manual(values = party_colors) +
  expand_limits(x = pop_max, 
                y = black_max) +
  labs(x = x_label,
       y = y_label,
       color = "Flipped to",
       title = "Counties that flipped shown by party color, and labeled by state",
       caption = "Counties in gray did not flip.")


## Zoom in -- replace expand_limits() with coord_cartesian()
## Adjust repel criteria also
p_layer_6 <- ggplot(data = subset(county_data,
                     flipped == "No"),
                    mapping = aes(x = pop, y = black/100)) + 
  geom_point(color = "gray30", 
             alpha = 0.25, 
             size = data_point_size) + 
  geom_point(data = subset(county_data,
                      flipped == "Yes"),
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16), 
             size = rel(2)) + 
  geom_label_repel(data = subset(county_data,
              flipped == "Yes" & black  
              > 20 & black < 50),
                  mapping = aes(x = pop,
                    y = black/100, label = state, 
                    family = "Tenso Slide", 
                    face = "bold"), size = rel(3.5)) + 
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) + 
  scale_y_continuous(breaks = y_breaks, 
                     labels = label_percent()) +
  scale_color_manual(values = party_colors) +
  coord_cartesian(xlim = c(0.75e4, 1.5e5), 
                  ylim = c(0.2, 0.5)) +
  labs(x = x_label,
       y = y_label,
       color = "Flipped to",
       title = "Counties that flipped shown by party color, and labeled by state; zoomed-in",
       caption = "Counties in gray did not flip.")





## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-18"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_1


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-19"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_2


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-20"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_3


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-21"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_4


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-22"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_5


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-23"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_6


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-24"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_5


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-25"
kjh_set_classic_theme(3)


## -----------------------------------------------------------------------------
#| label: "codefig-themes1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p <- organdata |> 
  drop_na(world) |> 
  ggplot(mapping = aes(x = roads, y = donors, 
                          color = world)) + 
  geom_point(size = 3) + 
  labs(x = "Road Deaths", 
       y = "Procurement Rate",
       title = "By Welfare State Regime")

p 



## -----------------------------------------------------------------------------
#| label: "codefig-theme2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p + theme_bw()


## -----------------------------------------------------------------------------
#| label: "codefig-theme3"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p + theme_minimal()


## -----------------------------------------------------------------------------
#| label: "codefig-themedark"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p + theme_dark()


## -----------------------------------------------------------------------------
#| label: "codefig-themeadditive"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p + theme_bw() +
  theme(legend.position = "top")



## -----------------------------------------------------------------------------
#| label: "codefig-elementtext"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p + theme(plot.title = 
            element_text(size = rel(3),
                         face = "bold", 
                         color = "orange"))



## -----------------------------------------------------------------------------
#| label: "codefig-elementline"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p + theme(panel.grid.major.y = 
            element_line(color = "red"),
          panel.grid.minor.y = 
            element_line(color = "black", 
                         linetype = "dotted"))



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-33"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
p_layer_4


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-34"
library(ggthemes)
theme_set(theme_fivethirtyeight())



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-35"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
p_layer_4


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-36"
theme_set(theme_economist())



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-37"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
p_layer_4


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-38"
theme_set(theme_stata())



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-39"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
p_layer_4



## ----09-case-studies-1, message = FALSE---------------------------------------
library(here)       # manage file paths
library(tidyverse)  # your friend and mine
library(cavax)      # california vaccination exemption data
library(colorspace) # luminance-balanced palettes
library(demog)      # demographic data for a graph
library(ggforce)    # useful enhancements to ggplot
library(ggrepel)    # Text and labels
library(gssr)       # the gss packaged for r
library(patchwork)  # compose multiple plots
library(scales)     # scale adjustments and enhancements
library(socviz)     # data and some useful functions



## ----09-case-studies-2, echo = FALSE------------------------------------------
### Quick convenience function, as we're going to make this plot several
### times.
two.y <- function(x, y1, y2,
                  y1.lim = range(y1),
                  y2.lim = range(y2),
                  y2.lab = "Billions of Dollars",
                  ttxt = NULL,
                  ...) {

    ## y1.lim <- range(y1)
    ## y2.lim <- range(y2)
    y1.lo <- y1.lim[1]
    y1.hi <- y1.lim[2]
    y2.lo <- y2.lim[1]
    y2.hi <- y2.lim[2]

    par(mar=c(5,4,4,5)+.1)
    plot(x, y1,
         type="l",
         lwd = 2,
         col="deepskyblue4",
         xlab="Date",
         ylab="S&P Points",
         ylim=c(y1.lo-100, y1.hi+100))

    par(new=TRUE)

    plot(x, y2, type="l",
         col="firebrick",
         lwd = 2,
         xaxt="n",
         yaxt="n",
         xlab="",
         ylab="",
         ylim=c(y2.lo, y2.hi))
    title(main = ttxt)

    axis(4)

    mtext(y2.lab, side=4, line=3)
    legend("topleft",
           col=c("deepskyblue4","firebrick"),
           bty="n", lty=1,
           legend=c("S&P 500", "Monetary Base"))


}



## ----09-case-studies-3, echo = FALSE, fig.width=12, fig.height=6--------------
par(mar=c(0,0,0,0)+.1)
two.y(x=fredts$date,
      y1=fredts$sp500,
      y2=fredts$monbase/1000,
      ttxt = "")


## ----09-case-studies-4, echo = FALSE, fig.width=12, fig.height=6--------------
## 2. Change an axis
two.y(x=fredts$date,
      y1=fredts$sp500,
      y2=fredts$monbase/1000,
      y1.lim = c(696, 2126),
      y2.lim = c(0, 5000),
      ttxt = "Start y2 at Zero")



## ----09-case-studies-5, echo = FALSE, fig.width=12, fig.height=6--------------
## 3. Change y1 axis limits
two.y(x=fredts$date,
      y1=fredts$sp500,
      y2=fredts$monbase/1000,
      y1.lim = c(0, 4000),
      ttxt = "Start y1 at Zero; Max both at Max y2")



## ----09-case-studies-6--------------------------------------------------------
fredts <- as_tibble(fredts)
fredts


## ----reveal-fredpivot, include = FALSE----------------------------------------

fredts |> 
  select(date, sp500_i, monbase_i)  |> 
  pivot_longer(sp500_i:monbase_i, 
               names_to = "series", 
               values_to = "score") ->
  fredts_m



## ----reveal-fredts1, include = FALSE------------------------------------------
fredts_m |> 
  ggplot(mapping = 
           aes(x = date, 
               y = score,
               color = series)) + 
  geom_line(size = 2) + 
  labs(x = "Date", y = "Index",
        color = "Series") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) -> 
  p1
 
# The original df
fredts |> 
  ggplot(mapping = 
          aes(x = date, 
              y = sp500_i - monbase_i)) + 
  geom_line(size = 1.5) + 
  labs(x = "Date", y = "Difference") -> 
  p2



## ----reveal-patchwork, include = FALSE----------------------------------------
library(patchwork)

(p1 / p2) + 
  plot_layout(heights = c(4, 1)) + 
  plot_annotation(title = "Index and Difference") ->
  p_patch


## ----09-case-studies-7, echo = FALSE, fig.width=12, fig.height=8--------------
p_patch


## ----09-case-studies-8--------------------------------------------------------
yahoo


## ----reveal-yahoo, include = FALSE--------------------------------------------
 
yahoo |> 
  ggplot(mapping = 
           aes(x = Employees, 
               y = Revenue)) +
  geom_path(color = "gray40", 
            size = rel(2)) +
  geom_label(aes(color = Mayer, 
                label = Year),
            size = rel(5), 
            fontface = "bold") +
  scale_y_continuous(labels = label_dollar()) +
  scale_x_continuous(labels = label_comma()) + 
  theme(legend.position = "bottom") +
  labs(color = "Mayer is CEO",
       x = "Employees", y = "Revenue (Millions)",
       title = "Yahoo Employees vs Revenues, 2004-2014") ->
  yahoo1




## ----09-case-studies-9, echo = FALSE, fig.width=10, fig.height=8--------------
yahoo1


## ----reveal-yahooalt, include = FALSE-----------------------------------------
yahoo |> 
  ggplot(mapping = 
           aes(x = Year, 
               y = Revenue/Employees)) + 
  geom_vline(xintercept = 2012, 
             size = rel(0.5), 
             linetype = "dotted") +
  geom_line(color = "royalblue", size = rel(2)) +
  annotate("text", x = 2012.6, y = 0.44,
           label = "Mayer\n becomes\n CEO", size = rel(5)) +
  labs(title = "Yahoo Revenue to Employee Ratio, 2004-2014",
       x = "Year",
       y = "Revenue/Employees") ->
  yahoo2


## ----09-case-studies-10, echo = FALSE, fig.width=10, fig.height = 6-----------
yahoo2


## ----09-case-studies-11-------------------------------------------------------
studebt


## ----09-case-studies-12-------------------------------------------------------
p_ylab <- "Amount Owed, in thousands of Dollars"
p_title <- "Outstanding Student Loans"
p_subtitle <- "44 million borrowers owe a total of $1.3 trillion"
p_caption <- "Source: FRB NY"

studebt <- studebt |> 
  mutate(type_label = recode(type, "Borrowers" = "Percent of all Borrowers",
                        "Balances" = "Percent of all Balances"))

studebt


## ----reveal-debt, include = FALSE---------------------------------------------
studebt |> 
  ggplot(mapping = 
           aes(x = pct/100, 
               y = Debt, 
               fill = type)) + 
  geom_col() +
  scale_fill_brewer(type = "qual", 
                    palette = "Dark2") +
  scale_x_continuous(labels = label_percent()) +
  guides(fill = "none") +
  labs(x = "Percent", 
       y = p_ylab,
       caption = p_caption,
       title = p_title,
       subtitle = p_subtitle) +
  facet_wrap(~ type_label, 
             labeller = 
               label_wrap_gen(width=10)) + 
    theme(strip.text.x = 
          element_text(face = "bold")) ->
  p1_debt


## ----09-case-studies-13, echo = FALSE, fig.width=10, fig.height=8-------------
p1_debt


## ----reveal-debtalt, include = FALSE------------------------------------------
studebt |> 
  ggplot(mapping = aes(x = pct/100, 
                       y = type_label, 
                       fill = Debtrc)) + 
  geom_col(color = "gray80") +
  scale_x_continuous(labels = 
                       label_percent()) +
  scale_fill_viridis_d() +
  guides(fill = 
           guide_legend(reverse = TRUE,
                        title.position = "top",
                        label.position = "bottom",
                        keywidth = 3,
                        nrow = 1)) +
  labs(x = NULL, y = NULL,
       fill = "Amount Owed, in thousands of dollars",
       caption = p_caption, title = p_title,
       subtitle = p_subtitle) +
  theme(legend.position = "top",
        plot.title = element_text(size = rel(2.8)),
        axis.text = element_text(face = "bold", 
                hjust = 1, 
                size = rel(2)),
          axis.ticks.length = unit(0, "cm"),
          axis.line = element_blank(), 
          panel.grid = element_blank()) ->
  p_debt2
 


## ----09-case-studies-14, echo = FALSE, fig.height=5, fig.width=20-------------
p_debt2


## ----09-case-studies-15-------------------------------------------------------
# remotes::install_github("kjhealy/demog")
# library(demog)

okboomer


## ----reveal-lineboom, include = FALSE-----------------------------------------
okboomer |> 
    filter(country == "United States")  |> 
    ggplot(aes(x = date, y = births_pct_day)) +
    geom_line(size = 0.5) +
    labs(x = "Year",
         y = "Average daily births per million") ->
  p_lineboom 


## ----09-case-studies-16, echo = FALSE, fig.width=20, fig.height=6-------------
p_lineboom 



## ----reveal-boomertile, include = FALSE---------------------------------------
okboomer |>
    mutate(year_fct = 
             factor(year,  
                    levels = unique(year),
                    ordered = TRUE),
           month_fct = factor(month,
                              levels = rev(c(1:12)),
                              labels = rev(c("Jan", "Feb", "Mar", "Apr",
                                  "May", "Jun", "Jul", "Aug",
                                  "Sep", "Oct", "Nov", "Dec")),
                              ordered = TRUE)) |>
    select(year, month, year_fct, month_fct, everything()) |> 
  filter(country == "United States") |>
    ggplot(aes(x = year_fct, y = month_fct)) +
    geom_tile(mapping = aes(fill = births_pct_day), 
              color = "white") + 
   scale_x_discrete(breaks = seq(1940, 2010, 5)) +    
   scale_fill_viridis_c(option = "B") + 
  labs(x = NULL, y = NULL, 
       title = "Monthly Birth Rates",
       fill = "Average births per million people per day",
         caption = "Data: US Census Bureau.") + 
  coord_fixed() +
  guides(fill = guide_legend(keywidth = 3, 
                    label.position = "bottom")) + 
  theme(legend.position = "bottom", 
        legend.justification = "left") ->
  p_tileboom



## ----09-case-studies-17, echo = FALSE, fig.height=4.8, fig.width=20-----------
p_tileboom


## ----09-case-studies-18-------------------------------------------------------
# remotes::install_github("kjhealy/cavax)
library(cavax)

cavax


## ----reveal-auxinfo, include = FALSE------------------------------------------
library(ggbeeswarm)
make_comma <- scales::label_comma()

cavax |>  
  group_by(mwc) |> 
  summarize(n_schools=n(), 
            n_students = sum(enrollment, na.rm=TRUE)) |> 
  drop_na() |> 
  mutate(n_schools_fmt = make_comma(n_schools), 
         n_students_fmt = make_comma(n_students),  
         info_schools = paste(n_schools_fmt, "Schools Enrolling"),
         info_students = paste(n_students_fmt, "Kindergarteners")) ->
  aux_info


## ----reveal-auxinfo2, include = FALSE-----------------------------------------
## This is not an efficient way to do this
aux_info |> 
  select(mwc, info_schools, info_students) |> 
  mutate(across(everything(), as.character)) |> 
  group_by(mwc) |> 
  group_keys() |> 
  pull() |> 
  as.character() ->
  keys

aux_info |> 
  select(mwc, info_schools, info_students) |>  
  mutate(across(everything(), as.character)) |> 
  group_split(mwc) |> 
  set_names(keys) |> # There's a better way ...
  map_chr(.f = paste, sep = "", collapse = "\n") ->
  special_x_labs 
 


## ----09-case-studies-19, echo = FALSE-----------------------------------------
kjh_set_classic_theme()
theme_set(theme_minimal())


## ----reveal-beemain, include = FALSE------------------------------------------
cavax |> 
  filter(mwc %nin% c("Private Christian Montessori", 
                     "Charter Montessori", 
                     "Private Jewish/Islamic")) |> 
  left_join(aux_info, by = "mwc") |> 
  ggplot(mapping = 
           aes(y = pbe_pct, 
               x = reorder(mwc, -n_students), 
               size = enrollment, 
               fill = mwc)) + 
  geom_quasirandom(shape=21, 
        alpha = 0.4,color="gray30",
        method = "quasirandom",
        varwidth = FALSE,
        bandwidth = 0.9) + 
      guides(color = "none",
          shape= "none", 
          fill= "none",
          size = guide_legend(override.aes =
                    list(fill = "black"))) +
      scale_size(breaks=c(20, 40, 75, 100, 300),
                   range=c(1,10)) +
      scale_x_discrete(labels = special_x_labs) + 
      labs(size = "Number of kindergarteners in each school",
           x = NULL, y = "Percent", 
           title = "Vaccination Exemptions in California Kindergartens") +
        theme(legend.position = "bottom", 
              plot.title = element_text(size = rel(1.4), 
                                        face = "bold")) ->
  p_bee_main
 


## ----09-case-studies-20, echo = FALSE, fig.height = 8, fig.width=15-----------
p_bee_main


Analysing the PublicHouse dataset, v3
================
James Gleeson
2022-09-18

## Introduction

The [PublicHouse dataset](https://github.com/jgleeson/PublicHouse) is a
compilation of publicly available data on the number of dwellings and
people in cities and countries around the world. The
[README](https://github.com/jgleeson/PublicHouse/blob/main/README.md)
provides details about the creation of the dataset, while this note sets
out some basic analysis using the R programming language. It also
includes the code used to carry out the analysis, so you can re-create
or adapt the charts.

This is the second edition of this analysis note, published in May 2022.
The dataset has been expanded and updated, but in a more significant
change the analysis uses interpolation to fill in gaps between data
points, which improves some of the charts.

Let’s start by loading the R packages we’re going to use.

``` r
library(tidyverse) # for various analysis functions
library(khroma) # for the Okabe-Ito colour scale
library(ggbraid) # for the 'braid' geom
library(ggdensity) # for shaded probability areas on scatterplot diagrams
library(geomtextpath) # for annotated line charts
library(lubridate) # for working with dates
library(zoo) # for time series interpolation
```

Another bit of setup is to set some chart design defaults.

``` r
theme_set(theme_minimal(base_size = 14))
theme_update(text = element_text(family = "Calibri"), 
             plot.title = element_text(size = 12, hjust = 0.5)) 
```

## Loading and examining the data

Now we load the dataset straight from Github.

``` r
d <- read_csv("https://raw.githubusercontent.com/jgleeson/PublicHouse/main/dataset.csv") %>%
  mutate(ref_date = dmy(ref_date)) 
```

As a first step, let’s see what places and years we have data for.
PublicHouse includes data on both cities and countries, so let’s start
with countries. Each dot is an observation.

``` r
d %>%
  filter(area_level != "city-region") %>%
  mutate(variable = str_to_title(variable)) %>%
  ggplot(aes(x = year, y = variable, colour = variable)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_colour_okabeito() +
  labs(x = NULL, y = NULL,
       title = "PublicHouse data coverage by country") +
  facet_wrap(~area_name) +
  theme(legend.position = "none",
        strip.background = element_rect(colour = "grey80", fill = "grey80"),
        panel.border = element_rect(colour = "grey80", fill = NA),
        panel.spacing.x = unit(15, units = "pt"))
```

<img src="Analysis_files/figure-gfm/coverage plot for countries-1.png" width="864" />

And here’s the same thing but for cities. The coverage is not as good
for cities as for countries, with fewer places and points in time
covered. Sometimes this is because there simply isn’t any sub-national
data reported, but often it’s because there is sub-national data but not
at a city level. Note, they’re not included in the chart below but I
have labelled Singapore and Hong Kong as ‘city states’ and included them
in both the country and city charts in the rest of the document.

``` r
d %>%
  filter(area_level == "city-region") %>%
  mutate(variable = str_to_title(variable)) %>%
  ggplot(aes(x = year, y = variable, colour = variable)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_colour_okabeito() +
  labs(x = NULL, y = NULL,
       title = "PublicHouse data coverage by city") +
  facet_wrap(~area_name) +
  theme(legend.position = "none",
        strip.background = element_rect(colour = "grey80", fill = "grey80"),
        panel.border = element_rect(colour = "grey80", fill = NA),
        panel.spacing.x = unit(15, units = "pt"))
```

<img src="Analysis_files/figure-gfm/coverage plot for cities-1.png" width="864" />
As you can see some data is annual while in other cases it is reported
every X number of years. When the data is intermittent the population
and dwelling reporting doesn’t always line up, so the next step
interpolates the ‘missing’ data points between reporting years. We’ll
use this interpolated dataset for some of the analysis that follows.

``` r
d_interpolated <- d %>%
  group_by(area_name, variable) %>% 
  complete(variable, year = min(year):max(year)) %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>% 
  fill(full_area_name:grouping, source_org:source_title, frequency:notes) 
```

## Dwellings and population in UK and France

Here’s a simple example of the kind of comparison that can be made with
this data, showing the trend in the population and the dwelling stock in
the UK and France.

``` r
d %>%
  filter(area_name %in% c("France", "UK")) %>%
  mutate(variable = str_to_title(variable)) %>%
  ggplot(aes(x = year, y = value, group = area_name, colour = area_name)) +
  geom_line() +
  scale_colour_okabeito() +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, NA)) +
  labs(x = NULL, y = NULL, colour = NULL,
       title = "Trend in population and number of dwellings in France and UK") +
  facet_wrap(~variable, scales = "free")
```

<img src="Analysis_files/figure-gfm/dwellings and population in UK and France-1.png" width="672" />

## Dwellings per person

That comparison broadly works for the UK and France because they have
similar population sizes. But given the differences in the populations
of all the countries we’re looking at, it doesn’t make much sense to
just compare them by their absolute number of dwellings, so instead we
calculate a measure of dwellings per person (or dwellings per 1,000
people in the charts below, for presentation purposes). This measures
the stock of housing, scaled to the population of the area. The inverse
of dwellings per person is persons per dwelling, which is similar to the
average household size - but household size is affected by things like
vacancy rates, second homes, sharing and the non-household population so
is harder to compare across countries.

Is a higher rate of dwellings per person a good thing? Well, housing is
a ‘normal’ good in the sense that people generally demand more of it as
their incomes increase, so within a given place we would expect to see
an increase over time.

The chart below shows the trend in dwellings per 1,000 people over time
for countries, colour-coded by a country grouping of my own devising
(which is inevitably slightly arbitrary - I’ve put France (and Paris) in
the ‘Southern Europe’ category, for example). Note, the x axis starts at
1980 as there are relatively few countries with data before that, and
the y axis starts at 200.

``` r
d %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  mutate(dpp = dwellings*1000/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  filter(area_level != "city-region") %>% 
  filter(year > 1979) %>%
  ggplot(aes(x = year, y = dpp, group = area_name, colour = grouping)) +
  geom_line(size = 0.75) +
  scale_colour_okabeito() +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  scale_y_continuous(limits = c(200, 650)) + 
  labs(x = NULL, y = NULL, title = "Dwellings per 1,000 people by country") +
  facet_wrap(~area_name) +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        strip.background = element_rect(colour = "grey80", fill = "grey80"),
        panel.border = element_rect(colour = "grey80", fill = NA),
        panel.spacing.x = unit(15, units = "pt"))
```

<img src="Analysis_files/figure-gfm/dwellings per person trend by country-1.png" width="864" />

A couple of patterns stand out here. First, there is a lot of variation
in both the level of dwellings per person and the rate of growth over
time. Second, in many countries there is a flattening of the curve in
roughly the second half of the period (since 2000), Spain and Ireland
being quite extreme examples. Third, East Asian and Southern European
countries have generally seen bigger increases over time, while
Anglophone countries haven’t seen much increase at all since 2000.

Next is the trend over time for cities. The slope of the lines for
cities are more often horizontal or downwards, reflecting the resurgence
in population growth in many cities and the failure to increase urban
housing stocks fast enough.

``` r
d %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  mutate(dpp = dwellings*1000/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  filter(area_level != "country") %>% 
  filter(year > 1979) %>%
  ggplot(aes(x = year, y = dpp, group = area_name, colour = grouping)) +
  geom_line(size = .75) +
  scale_colour_okabeito() +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  scale_y_continuous(limits = c(200, 650)) + 
  labs(x = NULL, y = NULL, title = "Dwellings per 1,000 people by city") +
  facet_wrap(~area_name) +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        strip.background = element_rect(colour = "grey80", fill = "grey80"),
        panel.border = element_rect(colour = "grey80", fill = NA),
        panel.spacing.x = unit(15, units = "pt"))
```

<img src="Analysis_files/figure-gfm/dwellings per person trend by city-1.png" width="864" />

Here’s another way of looking at the same data - taking the most recent
year’s figure for dwellings per 1,000 people (as long as there is one
more recently than 2015) and calculating the percentage change from a
decade earlier. The chart combines countries (in bold font) and cities,
again coloured according to country grouping. The vertical dark line
divides places where dwellings per person increased (to the right) from
places where it fell (to the left). The horizontal dark line divides
places with higher than average dwellings per person (above the line)
from places with lower than the average (below the line).

``` r
scatter_data <- d_interpolated %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
        names_from = variable,
        values_from = value) %>%
  mutate(dpp = dwellings*1000/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  group_by(area_name) %>%
  filter(max(year) > 2015) %>%
  filter(year == max(year) | (year == max(year)-10)) %>% 
  mutate(dpp_change = dpp/dpp[1]-1) %>%
  filter(dpp_change !=0) %>%
  ungroup()

dpp_average <- scatter_data %>% summarise(mean(dpp)) %>% as.double()

scatter_data %>%
  mutate(face = case_when(
    area_level != "city-region" ~ "bold",
    TRUE ~ "italic"
  )) %>%
  ggplot() +
  geom_text(aes(x = dpp_change, y = dpp, colour = grouping,
                label = area_name, fontface = face), size = 3) +
  geom_hdr(aes(x = dpp_change, y = dpp, fill = grouping),
               xlim = c(-0.08,0.20), ylim = c(250, 600),
           probs = c(0.75)) +
  geom_hline(yintercept = dpp_average) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(NA, 650)) +
  labs(x = "% change in 10 years", y = "Dwellings per person, latest year", title = "Dwellings per 1,000 people and change over 10 years (where available)", caption = "City names in italics, countries in bold") +
  scale_colour_okabeito() +
  scale_fill_okabeito() +
  theme(panel.grid.major.x = element_line(colour = "grey80"),
        panel.grid.major.y = element_line(colour = "grey90"),
        legend.position = "bottom",
        legend.title = element_blank())
```

<img src="Analysis_files/figure-gfm/dpp level and change scatter-1.png" width="768" />

There are some quite striking patterns here. First, all of the
Anglophone places have fewer dwellings per person than the average, and
around half of them have also seen declines over the last decade.
Southern European places almost all have more dwellings per person than
average and have seen increases (Madrid and Barcelona being the notable
exceptions). East Asian places have very different amounts of housing
per person, but all saw significant increases over the period
(particularly Singapore).

But it’s important to remember that the number of dwellings per person
can change due to shifts in population as well as net housing supply.
The following two charts separate out the contribution of changes in
population and changes in the housing stock.

``` r
d_interpolated %>%
  filter(area_level != "city-region") %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
        names_from = variable,
        values_from = value) %>%
  mutate(dpp = dwellings*1000/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  group_by(area_name) %>%
  filter(max(year) > 2015) %>%
  filter(year == max(year) | (year == max(year)-10)) %>% 
  mutate(dpp_change = dpp - dpp[1],
         dpp_constant_pop = dwellings*1000/population[1],
         "Contribution of dwelling change" = dpp_constant_pop-dpp[1],
         dpp_constant_dwellings = dwellings[1]*1000/population,
         "Contribution of population change" = dpp_constant_dwellings-dpp[1]) %>% 
  filter(dpp_change != 0) %>%
  select(-c(dwellings, population, dpp, dpp_constant_pop, dpp_constant_dwellings)) %>% 
  rename("Change in dwellings per 1,000 people" = dpp_change) %>%
  pivot_longer(-c(area_name, year, area_level, grouping), names_to = "variable", values_to = "value") %>%
  ungroup() %>%
  ggplot(aes(x = reorder(area_name, value), y = value, fill = variable)) +
  geom_col(data = . %>% filter(variable != "Change in dwellings per 1,000 people")) +
  geom_point(data = . %>% filter(variable == "Change in dwellings per 1,000 people"),
             shape = 21, size = 2) +
  coord_flip() +
  scale_fill_okabeito() +
  labs(x = NULL, y = NULL, title = "Contribution of population and housing stock changes to change in dwellings per 1,000 people - \nmost recent 10 years") +
  facet_grid(grouping ~ ., scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(8, "pt"),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "grey80", colour = "grey80"),
        panel.spacing.y = unit(10, "pt")) 
```

<img src="Analysis_files/figure-gfm/dpp change decomposition - countries-1.png" width="768" />

At the country level, we can see that only three countries (Poland,
Japan and Portugal) saw a fall in their populations over the decade,
contributing to an increase in dwellings per person. Unsurprisingly, no
countries saw a decrease in their housing stock. Population growth was
highest in the Anglophone countries, which contrbuted to their low or
negative growth in dwellings per person. East Asian countries had low
population growth but still added a significant number of new homes.

``` r
d_interpolated %>%
  filter(area_level != "country") %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
        names_from = variable,
        values_from = value) %>%
  mutate(dpp = dwellings*1000/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  group_by(area_name) %>%
  filter(max(year) > 2015) %>%
  filter(year == max(year) | (year == max(year)-10)) %>% 
  mutate(dpp_change = dpp - dpp[1],
         dpp_constant_pop = dwellings*1000/population[1],
         "Contribution of dwelling change" = dpp_constant_pop-dpp[1],
         dpp_constant_dwellings = dwellings[1]*1000/population,
         "Contribution of population change" = dpp_constant_dwellings-dpp[1]) %>% 
  filter(dpp_change != 0) %>%
  select(-c(dwellings, population, dpp, dpp_constant_pop, dpp_constant_dwellings)) %>% 
  rename("Change in dwellings per 1,000 people" = dpp_change) %>%
  pivot_longer(-c(area_name, year, area_level, grouping), names_to = "variable", values_to = "value") %>%
  ungroup() %>%
  ggplot(aes(x = reorder(area_name, value), y = value, fill = variable)) +
  geom_col(data = . %>% filter(variable != "Change in dwellings per 1,000 people")) +
  geom_point(data = . %>% filter(variable == "Change in dwellings per 1,000 people"),
             shape = 21, size = 2) +
  coord_flip() +
  scale_fill_okabeito() +
  labs(x = NULL, y = NULL, title = "Contribution of population and housing stock changes to change in dwellings per 1,000 people - \nmost recent 10 years") +
  facet_grid(grouping ~ ., scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(8, "pt"),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "grey80", colour = "grey80")) 
```

<img src="Analysis_files/figure-gfm/dpp change decomposition - cities-1.png" width="768" />

At the city level, Seoul was the only place with a shrinking population
(and it still built a lot of housing). The fastest growth in urban
populations was generally in the Central/Northern European cities, and
the lowest in Southern European cities (including Paris). Only in East
Asia did cities consistently manage to grow their housing stock faster
than their population.

The next chart plots all the country-level data on one chart and derives
an overall average trend for each country grouping. These average trends
are more representative for some groupings than for others: for example,
the East Asian group is quite small and very diverse, with huge
differences in trends between, say, Singapore and Japan. The other key
point to note about these average trend lines is that they aren’t
weighted for the population size of each country - so in the Anglophone
grouping, for example, the data for the USA has no more effect on the
trend than the data for Ireland. I think that’s okay in this case as the
focus of this analysis is on comparing places.

With those caveats, this chart does reveal some striking general
patterns. In particular, there has on average been very little increase
in the amount of housing per person in the Anglophone countries since
the start of the 1980s, while there has been a steady increase in
Southern Europe, and increases over time in Central/Northern Europe and
East Asia.

``` r
d_interpolated %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  mutate(dpp = dwellings*1000/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  filter(area_level != "city-region") %>% 
  filter(year > 1979) %>%
  ggplot(aes(x = year, y = dpp, colour = grouping)) +
  geom_point(alpha = 0.1, size = 3) +
#  geom_smooth(se = F, span = 0.3) +
  geom_textsmooth(aes(x = year, y = dpp, colour = grouping, label = grouping), 
                  se = F, span = 0.3, method = "loess", size = 4, linewidth = 1) +
  scale_colour_okabeito() +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  coord_cartesian(ylim = c(200, 650)) + 
  labs(x = NULL, y = NULL, title = "Dwellings per 1,000 people by country grouping") +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey80"))
```

<img src="Analysis_files/figure-gfm/aggregate dwellings per person trend by country grouping-1.png" width="672" />

Finally, this chart shows an overall average trend for every country
except England, and compares that to the England trend. Broadly
speaking, England started out with more housing per person than the
international average, but has stagnated since 2000 and has ended up
with less.

``` r
d_interpolated %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  mutate(dpp = dwellings*1000/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  filter(area_level != "city-region") %>% 
  filter(year > 1959) %>%
  ggplot() +
  geom_smooth(data = . %>% filter(area_name != "England"), aes(x = year, y = dpp, colour = "International average trend"), span = 0.4) +
  geom_line(data = . %>% filter(area_name == "England"), 
            aes(x = year, y = dpp, colour = "England")) +
  scale_colour_okabeito() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020)) +
  labs(x = NULL, y = NULL, title = "Dwellings per 1,000 people - England and aggregate international trends") +
  theme(legend.position = "bottom",
        legend.title = element_blank())
```

<img src="Analysis_files/figure-gfm/dwellings per person trend by country with England focus-1.png" width="576" />

## Change in housing and population over time

The previous section disaggregated changes in dwellings per capita by
comparing growth in the population and housing stock over a period of
time. We can get a finer-grained understanding by tracking the
proportional change in housing and population from a particular
baseline. The chart below does this for countries from the year 2000 up
to the latest available year (mostly 2020).

``` r
d_interpolated %>%
  mutate(variable = str_to_title(variable)) %>%
  group_by(variable, area_name) %>%
  filter(year > 1999) %>%
  mutate(index = (value/value[1])*100) %>%
  filter(area_level != "city-region") %>%
  filter(area_name %in% c("Australia", "England", "Finland", "France", "Germany",
                          "Ireland", "Japan", "Netherlands", "New Zealand", 
                          "Poland", "Portugal", "Spain",
                          "Sweden", "Switzerland", "Taiwan", "USA")) %>%
  ggplot(aes(x = year, y = index, colour = variable,
             group = variable)) +
  geom_line() +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(breaks = c(100, 120, 140)) +
  scale_colour_okabeito() +
  labs(x = NULL, y = NULL,
       title = "Index of change in dwelling stock and population, selected countries \n(2000 = 100)") +
  facet_wrap(~area_name) +
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank())
```

<img src="Analysis_files/figure-gfm/indexed change - countries-1.png" width="672" />

The next chart does the same thing for cities. You’ll note that there
are more cities than countries where growth in the housing stock failed
to keep up with or only barely kept up with growth in the population.

``` r
d_interpolated %>%
  mutate(variable = str_to_title(variable)) %>%
  group_by(variable, area_name) %>%
  filter(year > 1999) %>%
  mutate(index = (value/value[1])*100) %>%
  filter(area_level != "country") %>%
  filter(!area_name %in% c("Seoul", "Amsterdam", "Lisbon", "Copenhagen", "Oslo")) %>%
  ggplot(aes(x = year, y = index, colour = variable,
             group = variable)) +
  geom_line() +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(breaks = c(100, 120, 140)) +
  scale_colour_okabeito() +
  labs(x = NULL, y = NULL,
       title = "Index of change in dwelling stock and population, selected cities \n(2000 = 100)") +
  facet_wrap(~area_name) +
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank())
```

<img src="Analysis_files/figure-gfm/indexed change - cities-1.png" width="672" />

## Dwelling growth compared to national income growth

We’ve compared housing growth with population growth in previous
sections because population data is easily available and it serves as an
obvious denominator for measuring housing growth against. But population
by itself is not a good measure of the *demand* for housing, since
factors such as the housing preferences, incomes and access to credit of
the population change over time.

There is probably no perfect single measure of demand for housing, but
the next chart compares change in the housing stock of a country with
change in its Gross National Income, as an admittedly imperfect proxy
for the buying/renting power of the population. To get this data and
match it against our existing data we use the
`[WDI](https://github.com/vincentarelbundock/WDI)` and
`[countrycode](https://github.com/vincentarelbundock/countrycode)`
packages by Vincent Arel-Bundock.

``` r
library(countrycode) # for matching country names to the World Bank data
library(WDI) # for World Bank data

# get GNI data by country and year
gni <- WDI(indicator = "NY.GNP.MKTP.PP.KD", start = 2000, end = 2020)

# assign country codes
d_codes <- d_interpolated %>%
  filter(area_level != "city-region") %>%
  mutate(code = countrycode(area_name, origin = "country.name", destination = "iso2c")) %>%
  mutate(code = case_when(area_name == "England" ~ "GB", TRUE ~ code)) %>% # We use UK GDP for England (the trend growth will be similar even if the level isn't)
  left_join(gni, by = c("code" = "iso2c", "year" = "year")) 

# filter to the data we're interested in and calculate index trends
d_codes <- d_codes %>% 
  filter(variable == "dwellings") %>%
  filter(year > 1999) %>%
  group_by(area_name) %>%
  mutate(dwellings = (value/value[1])*100) %>% 
  mutate(GNI = (NY.GNP.MKTP.PP.KD/NY.GNP.MKTP.PP.KD[1])*100) %>%
  filter(area_name %in% c("Austria", "England", "Finland", "France", "Germany",
                          "Ireland", "Japan", "Netherlands", "New Zealand", 
                          "Poland", "Portugal", "Spain", "Belgium",
                          "Sweden", "Switzerland", "USA")) %>%
  select(area_name, year, "Dwellings" = dwellings, GNI) 

# create a 'long' version for plotting purposes
d_codes_long <- d_codes %>%
  pivot_longer(cols = c(Dwellings, GNI), names_to = "variable", values_to = "value")  

# plot it
ggplot() +
  geom_line(data = d_codes_long, aes(x = year, y = value, colour = variable, group = variable)) +
  geom_braid(data = d_codes, aes(x = year, ymin = Dwellings, ymax = GNI, fill = Dwellings < GNI),
             alpha = 0.4) +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  scale_colour_okabeito() +
  scale_fill_okabeito() +
  labs(x = NULL, y = NULL, title = "Index of change in number of dwellings and Gross National Income, selected countries \n (2000 = 100)") +
  guides(fill = "none") +
  facet_wrap(~area_name, scales = "free") +
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank())
```

<img src="Analysis_files/figure-gfm/dwelling and GNP growth-1.png" width="672" />

## Net additional dwellings per 1,000 population

There are a number of other measures of housing stock or supply you
could use, but this section focuses on the net annualised change in
dwellings per 1,000 population. Whereas dwellings per capita is a
measure of the *stock* of housing, this is a measure of the *flow*
(albeit in net rather than gross terms), and it is not affected by
either the previous size of the stock or by the change in population (at
least not very much). So countries that are building a lot of homes
relative to their population *size* can ‘look good’ by this measure even
if that new supply isn’t actually enough to keep up with population
*growth*.

The first chart in this section shows this measure for countries. The
trend in most countries actually looks fairly flat by this measure,
largely because of some extremely high values in a few countries
(notably Ireland and South Korea)

``` r
d %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  group_by(area_name) %>%
  mutate(net_change = (dwellings - lag(dwellings, 1))/(year - lag(year, 1)),
         net_change_per_1k = net_change/((population + lag(population, 1))/2)*1000) %>%
  filter(!is.na(net_change_per_1k)) %>%
  filter(area_level != "city-region") %>% 
  filter(year > 1979) %>%
  ggplot(aes(x = year, y = net_change_per_1k, group = area_name, colour = grouping)) +
  geom_line(size = 0.75) +
  scale_colour_okabeito() +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  scale_y_continuous(limits = c(0,NA)) +
  labs(x = NULL, y = NULL, title = "Annualised net additional dwellings per 1,000 people by country") +
  facet_wrap(~area_name) +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        strip.background = element_rect(colour = "grey80", fill = "grey80"),
        panel.border = element_rect(colour = "grey80", fill = NA),
        panel.spacing.x = unit(15, units = "pt"))
```

<img src="Analysis_files/figure-gfm/annualised net dwellings per capita by country-1.png" width="864" />

Here’s the same chart for cities. Again the extreme outliers (hello
Seoul!) tend to flatten the trend for everywhere else. The breaks in the
trends for Berlin and Brussels are due to the net dwelling change
briefly going negative - I suspect due to data collection reasons rather
than an actual shrinkage of the housing stock.

``` r
d %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  group_by(area_name) %>%
  mutate(net_change = (dwellings - lag(dwellings, 1))/(year - lag(year, 1)),
         net_change_per_1k = net_change/((population + lag(population, 1))/2)*1000) %>%
  filter(!is.na(net_change_per_1k)) %>%
  filter(area_level != "country") %>% 
#  filter(area_name %in% c("London", "Paris", "New York", "Tokyo", "Berlin", "Singapore")) %>%
  filter(year > 1979) %>%
  ggplot(aes(x = year, y = net_change_per_1k, group = area_name, colour = grouping)) +
  geom_line(size = 0.75) +
#  geom_point(size = 2.5) +
  scale_colour_okabeito() +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  labs(x = NULL, y = NULL, title = "Annualised net additional dwellings per 1,000 people by city") +
  facet_wrap(~area_name) +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        strip.background = element_rect(colour = "grey80", fill = "grey80"),
        panel.border = element_rect(colour = "grey80", fill = NA),
        panel.spacing.x = unit(15, units = "pt"))
```

<img src="Analysis_files/figure-gfm/net dwellings per capita by city-1.png" width="864" />

Can we extract any aggregate trends? The chart below shows smoothed
averages for country groupings, but as the quite widely dispersed dots
around each trend should suggest, these are fairly indicative trends at
best. Broadly speaking it looks like new supply peaked in East Asian
around 2000 (possibly as a result of the late 1990s financial crisis in
that region), while there was a peak in supply around the time of the
global financial crisis in Anglophone countries and (a much stronger
effect) in Southern European countries. The European and (in particular)
the Anglophone countries have increased supply in recent years while in
East Asia supply has fallen.

``` r
d %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  group_by(area_name) %>%
  mutate(net_change = (dwellings - lag(dwellings, 1))/(year - lag(year, 1)),
         net_change_per_1k = net_change/((population + lag(population, 1))/2)*1000) %>%
  filter(!is.na(net_change_per_1k)) %>%
  filter(area_level != "city-region") %>% 
  filter(year > 1979) %>%
  ggplot(aes(x = year, y = net_change_per_1k, colour = grouping)) +
  geom_point(alpha = 0.1, size = 3) +
  geom_smooth(se = FALSE, span = 0.4) +
  scale_colour_okabeito() +
  coord_cartesian(ylim = c(0, 10)) + # use this rather than scale_x_continuous to 'zoom' into a section of the chart without losing any data 
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  labs(x = NULL, y = NULL, title = "Annualised net additional dwellings per 1,000 people by country") +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey80"))
```

<img src="Analysis_files/figure-gfm/summary net additions chart by country-1.png" width="576" />

And finally, here’s the trend for England compared to the aggregate
trend for all other countries. At the international level, new supply
seemed to peak in the late 60s / early 70s, then was relatively steady
from the early 80s through to the mid 2000s, but took a serious hit as a
result of the global financial crisis. England has had lower net housing
supply per person than the international trend throughout this period,
and the long-term trend has been generally downwards.

``` r
d_interpolated %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  group_by(area_name) %>%
  mutate(net_change = (dwellings - lag(dwellings, 1))/(year - lag(year, 1)),
         net_change_per_1k = net_change/((population + lag(population, 1))/2)*1000) %>%
  filter(!is.na(net_change_per_1k)) %>%
  filter(area_level != "city-region") %>% 
  filter(year > 1959) %>%
  ggplot() +
  geom_smooth(data = . %>% filter(area_name != "England"), 
              aes(x = year, y = net_change_per_1k, 
                  colour = "International average trend"), span = 0.4) +
  geom_line(data = . %>% filter(area_name == "England"),
            aes(x = year, y = net_change_per_1k, colour = "England")) +
  scale_colour_okabeito() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020)) +
  labs(x = NULL, y = NULL, title = "Annualised net additional dwellings per 1,000 people - \nEngland and aggregate international trends") +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey80"))
```

<img src="Analysis_files/figure-gfm/summary net additions chart with England focus-1.png" width="576" />

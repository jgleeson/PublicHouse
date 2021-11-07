Analysing the Openhouse dataset
================
James Gleeson
November 2021

## Introduction

The [Openhouse dataset](https://github.com/jgleeson/openhouse) is a
compilation of publicly available data on the number of dwellings and
people in cities and countries around the world. The
[README](https://github.com/jgleeson/openhouse/blob/main/README.md)
provides details about the creation of the dataset, while this note sets
out some basic analysis.

Let’s start by loading the R packages we’re going to use. We’re not
going to be doing anything very complicated so we can mainly use the
[tidyverse](https://www.tidyverse.org/) plus
[ggthemes](https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/)
for styling.

``` r
library(tidyverse)
library(ggthemes)
library(lubridate)
library(plotly)
```

Another bit of setup is to set the chart theme to use.

``` r
theme_set(theme_few())
```

## Loading and examining the data

Now let’s load the data.

``` r
d <- read_csv("https://raw.githubusercontent.com/jgleeson/openhouse/main/dataset.csv") %>%
  mutate(ref_date = dmy(ref_date))
```

As a first step, let’s see what places and dates we have data for.
Openhouse includes data on both cities and countries, so let’s start
with countries. Each dot is an observation.

``` r
d %>%
  filter(area_level != "city-region") %>%
  ggplot(aes(x = year, y = variable, colour = variable)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~area_name) +
  theme(legend.position = "none")
```

![](Analysis_files/figure-gfm/coverage%20plot%20for%20countries-1.png)<!-- -->

And here’s the same thing but for cities.

``` r
d %>%
  filter(area_level == "city-region") %>%
  ggplot(aes(x = year, y = variable, colour = variable)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~area_name) +
  theme(legend.position = "none")
```

![](Analysis_files/figure-gfm/coverage%20plot%20for%20cities-1.png)<!-- -->

## Dwellings per person

``` r
static <- d %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  mutate(dpp = dwellings/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  filter(area_level != "city-region") %>% 
  ggplot(aes(x = year, y = dpp, group = area_name, colour = area_name)) +
#  geom_point() +
  geom_line() +
  labs(x = NULL, y = NULL, title = "Dwellings per person by country") +
  facet_wrap(~grouping) +
  theme()
static
```

![](Analysis_files/figure-gfm/dwellngs%20per%20person-1.png)<!-- -->
That’s a bit busy so let’s see if we can do a plotly version

``` r
dynamic <- ggplotly(static)
```

<iframe src="https://raw.githubusercontent.com/jgleeson/openhouse/main/index.html" width="100%" height="600" scrolling="no" seamless="seamless" frameBorder="0"></iframe>

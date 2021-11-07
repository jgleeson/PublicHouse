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
library(ggiraph)
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
d %>%
  pivot_wider(id_cols = c(area_name, year, area_level),
              names_from = variable,
              values_from = value) %>%
  mutate(dpp = dwellings/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  filter(area_level != "city-region") %>% 
  ggplot(aes(x = year, y = dpp, group = area_name, colour = area_name)) +
  geom_point() +
  geom_line() +
  labs(x = NULL, y = NULL, title = "Dwellings per person by country") +
  theme()
```

![](Analysis_files/figure-gfm/dwellngs%20per%20person-1.png)<!-- -->

That’s a bit busy so let’s use interactivity to allow the chart to be
more easily read. This is courtesy of the [ggiraph
package](https://davidgohel.github.io/ggiraph/).

``` r
p <- d %>%
  pivot_wider(id_cols = c(area_name, year, area_level),
              names_from = variable,
              values_from = value) %>%
  mutate(dpp = dwellings/population) %>%
  arrange(area_name, year) %>%
  filter(!is.na(dpp)) %>%
  filter(area_level != "city-region") %>% 
  ggplot(aes(x = year, y = dpp, group = area_name, colour = area_name,
             tooltip = area_name, data_id = area_name)) +
  geom_line_interactive() +  
  geom_point() +
  labs(x = NULL, y = NULL, title = "Dwellings per person by country") +
  theme()

girafe(p)
```

<div id="htmlwidget-a580aefd6b720a0cddd7" style="width:672px;height:480px;" class="girafe html-widget"></div>
<script type="application/json" data-for="htmlwidget-a580aefd6b720a0cddd7">{"x":{"html":"","js":null,"uid":"svg_139276aa-cd8d-4710-b468-cc4dcb56eb73","ratio":1.2,"settings":{"tooltip":{"css":".tooltip_SVGID_ { padding:5px;background:black;color:white;border-radius:2px 2px 2px 2px;text-align:left; ; position:absolute;pointer-events:none;z-index:999;}\n","placement":"doc","offx":10,"offy":0,"use_cursor_pos":true,"opacity":0.9,"usefill":false,"usestroke":false,"delay":{"over":200,"out":500}},"hover":{"css":".hover_SVGID_ { fill:orange;stroke:gray; }","reactive":false},"hoverkey":{"css":".hover_key_SVGID_ { stroke:red; }","reactive":false},"hovertheme":{"css":".hover_theme_SVGID_ { fill:green; }","reactive":false},"hoverinv":{"css":""},"zoom":{"min":1,"max":1},"capture":{"css":".selected_SVGID_ { fill:red;stroke:gray; }","type":"multiple","only_shiny":true,"selected":[]},"capturekey":{"css":".selected_key_SVGID_ { stroke:gray; }","type":"single","only_shiny":true,"selected":[]},"capturetheme":{"css":".selected_theme_SVGID_ { stroke:gray; }","type":"single","only_shiny":true,"selected":[]},"toolbar":{"position":"topright","saveaspng":true,"pngname":"diagram"},"sizing":{"rescale":true,"width":1}}},"evals":[],"jsHooks":[]}</script>

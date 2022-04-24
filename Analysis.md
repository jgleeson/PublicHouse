## Introduction

The [PublicHouse dataset](https://github.com/jgleeson/PublicHouse) is a
compilation of publicly available data on the number of dwellings and
people in cities and countries around the world. The
[README](https://github.com/jgleeson/PublicHouse/blob/main/README.md)
provides details about the creation of the dataset, while this note sets
out some basic analysis using the R programming language. It also
includes the code used to carry out the analysis, so you can re-create
or adapt the charts.

This is the second edition of this analysis note, published in April
2022. The dataset has been expanded and updated, but in a more
significant change the analysis uses interpolation to fill in gaps
between data points, which improves some of the charts.

Let’s start by loading the R packages we’re going to use.

``` r
library(tidyverse) # for various analysis functions
library(khroma) # for the Okabe-Ito colour scale
library(ggbraid) # for the 'braid' geom
library(lubridate) # for working with dates
library(zoo) # for time series interpolation
library(gt) # for making tables
library(gtExtras) # additional gt functions
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

<img src="Analysis_files/figure-markdown_github/coverage plot for countries-1.png" width="864" />

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

<img src="Analysis_files/figure-markdown_github/coverage plot for cities-1.png" width="864" />
As you can see some data is annual while in other cases it is reported
every X number of years. When the data is intermittent the population
and dwelling reporting doesn’t always line up, so the next step
interpolates the ‘missing’ data points between reporting years. I’ll use
this interpolated dataset for some of the analysis that follows.

``` r
d_interpolated <- d %>%
  group_by(area_name, variable) %>%
  complete(variable, year = min(year):max(year)) %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>% 
  fill(full_area_name:grouping, source_org:source_title, frequency:notes) 
```

## Dwellings per person

A good place to start is by looking at how much housing there is to go
around in each country or city. But it wouldn’t make much sense to just
measure the number of dwellings, so instead we calculate a measure of
dwellings per person (or dwellings per 1,000 people in the charts below,
for presentation purposes). This measures the stock of housing, scaled
to the population of the area. The inverse of dwellings per person is
persons per dwelling, which is similar to the average household size -
but household size is affected by vacancy, second homes and sharing so
is harder to measure.

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

<img src="Analysis_files/figure-markdown_github/dwellings per person trend by country-1.png" width="864" />

A couple of patterns stand out here. First, there is a lot of variation
in both the level of dwellings per person and the rate of growth over
time. Second, in many countries there is a flattening of the curve in
roughtly the second half of the period (since 2000), Spain and Ireland
being quite extreme examples. Third, East Asian and Southern European
countries have generally seen bigger increases over time, while
Anglophone countries haven’t seen much increase at all since 2000.

Here’s a table ranking countries by their latest DPP.

``` r
d %>%
  filter(area_level != "city-region") %>%
  pivot_wider(id_cols = c(area_name, year, area_level, grouping),
              names_from = variable,
              values_from = value) %>%
  mutate(dpp = dwellings*1000/population) %>%
  filter(!is.na(dpp)) %>%
  group_by(area_name) %>%
  slice_max(year) %>%
  select(-c(area_level, grouping)) %>%
  ungroup() %>%
  gt() %>%
  gt_theme_guardian()
```

<div id="iktyprkggz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Noto+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
html {
  font-family: 'Noto Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#iktyprkggz .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #F6F6F6;
  width: auto;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #40C5FF;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#iktyprkggz .gt_heading {
  background-color: #F6F6F6;
  text-align: left;
  border-bottom-color: #F6F6F6;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#iktyprkggz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #F6F6F6;
  border-bottom-width: 0;
}

#iktyprkggz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #F6F6F6;
  border-top-width: 0;
}

#iktyprkggz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 0px;
  border-bottom-color: #D3D3D3;
}

#iktyprkggz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #40C5FF;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #ECECEC;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#iktyprkggz .gt_col_heading {
  color: #333333;
  background-color: #F6F6F6;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#iktyprkggz .gt_column_spanner_outer {
  color: #333333;
  background-color: #F6F6F6;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#iktyprkggz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#iktyprkggz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#iktyprkggz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #ECECEC;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#iktyprkggz .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #F6F6F6;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #BEBEBE;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #BEBEBE;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#iktyprkggz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #F6F6F6;
  font-size: 100%;
  font-weight: bold;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #BEBEBE;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #BEBEBE;
  vertical-align: middle;
}

#iktyprkggz .gt_from_md > :first-child {
  margin-top: 0;
}

#iktyprkggz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#iktyprkggz .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: white;
  border-top-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#iktyprkggz .gt_stub {
  color: #333333;
  background-color: #F6F6F6;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#iktyprkggz .gt_stub_row_group {
  color: #333333;
  background-color: #F6F6F6;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#iktyprkggz .gt_row_group_first td {
  border-top-width: 1px;
}

#iktyprkggz .gt_summary_row {
  color: #333333;
  background-color: #F6F6F6;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#iktyprkggz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#iktyprkggz .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#iktyprkggz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#iktyprkggz .gt_grand_summary_row {
  color: #333333;
  background-color: #F6F6F6;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#iktyprkggz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#iktyprkggz .gt_striped {
  background-color: #ECECEC;
}

#iktyprkggz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #FFFFFF;
}

#iktyprkggz .gt_footnotes {
  color: #333333;
  background-color: #F6F6F6;
  border-bottom-style: none;
  border-bottom-width: 0px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#iktyprkggz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#iktyprkggz .gt_sourcenotes {
  color: #333333;
  background-color: #F6F6F6;
  border-bottom-style: none;
  border-bottom-width: 0px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#iktyprkggz .gt_sourcenote {
  font-size: 12px;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#iktyprkggz .gt_left {
  text-align: left;
}

#iktyprkggz .gt_center {
  text-align: center;
}

#iktyprkggz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#iktyprkggz .gt_font_normal {
  font-weight: normal;
}

#iktyprkggz .gt_font_bold {
  font-weight: bold;
}

#iktyprkggz .gt_font_italic {
  font-style: italic;
}

#iktyprkggz .gt_super {
  font-size: 65%;
}

#iktyprkggz .gt_two_val_uncert {
  display: inline-block;
  line-height: 1em;
  text-align: right;
  font-size: 60%;
  vertical-align: -0.25em;
  margin-left: 0.1em;
}

#iktyprkggz .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#iktyprkggz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#iktyprkggz .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#iktyprkggz .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#iktyprkggz .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">area_name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">year</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dwellings</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">population</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dpp</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left" style="border-top-width: 0px; border-top-style: solid; border-top-color: white;">Australia</td>
<td class="gt_row gt_right" style="border-top-width: 0px; border-top-style: solid; border-top-color: white;">2021</td>
<td class="gt_row gt_right" style="border-top-width: 0px; border-top-style: solid; border-top-color: white;">10675900</td>
<td class="gt_row gt_right" style="border-top-width: 0px; border-top-style: solid; border-top-color: white;">25739256</td>
<td class="gt_row gt_right" style="border-top-width: 0px; border-top-style: solid; border-top-color: white;">414.7711</td></tr>
    <tr><td class="gt_row gt_left gt_striped">Austria</td>
<td class="gt_row gt_right gt_striped">2020</td>
<td class="gt_row gt_right gt_striped">4875111</td>
<td class="gt_row gt_right gt_striped">8916815</td>
<td class="gt_row gt_right gt_striped">546.7323</td></tr>
    <tr><td class="gt_row gt_left">Belgium</td>
<td class="gt_row gt_right">2021</td>
<td class="gt_row gt_right">5631637</td>
<td class="gt_row gt_right">11521238</td>
<td class="gt_row gt_right">488.8048</td></tr>
    <tr><td class="gt_row gt_left gt_striped">Canada</td>
<td class="gt_row gt_right gt_striped">2021</td>
<td class="gt_row gt_right gt_striped">16284235</td>
<td class="gt_row gt_right gt_striped">38246108</td>
<td class="gt_row gt_right gt_striped">425.7750</td></tr>
    <tr><td class="gt_row gt_left">Denmark</td>
<td class="gt_row gt_right">2021</td>
<td class="gt_row gt_right">3146511</td>
<td class="gt_row gt_right">5840045</td>
<td class="gt_row gt_right">538.7820</td></tr>
    <tr><td class="gt_row gt_left gt_striped">England</td>
<td class="gt_row gt_right gt_striped">2020</td>
<td class="gt_row gt_right gt_striped">24657900</td>
<td class="gt_row gt_right gt_striped">56550100</td>
<td class="gt_row gt_right gt_striped">436.0364</td></tr>
    <tr><td class="gt_row gt_left">Finland</td>
<td class="gt_row gt_right">2020</td>
<td class="gt_row gt_right">3124286</td>
<td class="gt_row gt_right">5533793</td>
<td class="gt_row gt_right">564.5831</td></tr>
    <tr><td class="gt_row gt_left gt_striped">France</td>
<td class="gt_row gt_right gt_striped">2021</td>
<td class="gt_row gt_right gt_striped">36247000</td>
<td class="gt_row gt_right gt_striped">65235843</td>
<td class="gt_row gt_right gt_striped">555.6301</td></tr>
    <tr><td class="gt_row gt_left">Germany</td>
<td class="gt_row gt_right">2020</td>
<td class="gt_row gt_right">42803737</td>
<td class="gt_row gt_right">83155031</td>
<td class="gt_row gt_right">514.7462</td></tr>
    <tr><td class="gt_row gt_left gt_striped">Hong Kong</td>
<td class="gt_row gt_right gt_striped">2021</td>
<td class="gt_row gt_right gt_striped">2942800</td>
<td class="gt_row gt_right gt_striped">7394700</td>
<td class="gt_row gt_right gt_striped">397.9607</td></tr>
    <tr><td class="gt_row gt_left">Ireland</td>
<td class="gt_row gt_right">2016</td>
<td class="gt_row gt_right">2003645</td>
<td class="gt_row gt_right">4761865</td>
<td class="gt_row gt_right">420.7690</td></tr>
    <tr><td class="gt_row gt_left gt_striped">Italy</td>
<td class="gt_row gt_right gt_striped">2016</td>
<td class="gt_row gt_right gt_striped">32100620</td>
<td class="gt_row gt_right gt_striped">60163712</td>
<td class="gt_row gt_right gt_striped">533.5545</td></tr>
    <tr><td class="gt_row gt_left">Japan</td>
<td class="gt_row gt_right">2018</td>
<td class="gt_row gt_right">62407400</td>
<td class="gt_row gt_right">126307500</td>
<td class="gt_row gt_right">494.0910</td></tr>
    <tr><td class="gt_row gt_left gt_striped">Netherlands</td>
<td class="gt_row gt_right gt_striped">2020</td>
<td class="gt_row gt_right gt_striped">7891786</td>
<td class="gt_row gt_right gt_striped">17407585</td>
<td class="gt_row gt_right gt_striped">453.3533</td></tr>
    <tr><td class="gt_row gt_left">New Zealand</td>
<td class="gt_row gt_right">2021</td>
<td class="gt_row gt_right">1954000</td>
<td class="gt_row gt_right">5112000</td>
<td class="gt_row gt_right">382.2379</td></tr>
    <tr><td class="gt_row gt_left gt_striped">Norway</td>
<td class="gt_row gt_right gt_striped">2021</td>
<td class="gt_row gt_right gt_striped">2637521</td>
<td class="gt_row gt_right gt_striped">5391369</td>
<td class="gt_row gt_right gt_striped">489.2117</td></tr>
    <tr><td class="gt_row gt_left">Poland</td>
<td class="gt_row gt_right">2020</td>
<td class="gt_row gt_right">15015333</td>
<td class="gt_row gt_right">38265013</td>
<td class="gt_row gt_right">392.4037</td></tr>
    <tr><td class="gt_row gt_left gt_striped">Portugal</td>
<td class="gt_row gt_right gt_striped">2020</td>
<td class="gt_row gt_right gt_striped">5983402</td>
<td class="gt_row gt_right gt_striped">10298252</td>
<td class="gt_row gt_right gt_striped">581.0114</td></tr>
    <tr><td class="gt_row gt_left">Singapore</td>
<td class="gt_row gt_right">2020</td>
<td class="gt_row gt_right">1490946</td>
<td class="gt_row gt_right">5685807</td>
<td class="gt_row gt_right">262.2224</td></tr>
    <tr><td class="gt_row gt_left gt_striped">South Korea</td>
<td class="gt_row gt_right gt_striped">2020</td>
<td class="gt_row gt_right gt_striped">21673500</td>
<td class="gt_row gt_right gt_striped">51829023</td>
<td class="gt_row gt_right gt_striped">418.1730</td></tr>
    <tr><td class="gt_row gt_left">Spain</td>
<td class="gt_row gt_right">2020</td>
<td class="gt_row gt_right">25882055</td>
<td class="gt_row gt_right">47450795</td>
<td class="gt_row gt_right">545.4504</td></tr>
    <tr><td class="gt_row gt_left gt_striped">Sweden</td>
<td class="gt_row gt_right gt_striped">2020</td>
<td class="gt_row gt_right gt_striped">5037444</td>
<td class="gt_row gt_right gt_striped">10379295</td>
<td class="gt_row gt_right gt_striped">485.3359</td></tr>
    <tr><td class="gt_row gt_left">Switzerland</td>
<td class="gt_row gt_right">2020</td>
<td class="gt_row gt_right">4637174</td>
<td class="gt_row gt_right">8729833</td>
<td class="gt_row gt_right">531.1870</td></tr>
    <tr><td class="gt_row gt_left gt_striped">Taiwan</td>
<td class="gt_row gt_right gt_striped">2020</td>
<td class="gt_row gt_right gt_striped">8992364</td>
<td class="gt_row gt_right gt_striped">23833611</td>
<td class="gt_row gt_right gt_striped">377.2976</td></tr>
    <tr><td class="gt_row gt_left">USA</td>
<td class="gt_row gt_right">2019</td>
<td class="gt_row gt_right">139684244</td>
<td class="gt_row gt_right">330513000</td>
<td class="gt_row gt_right">422.6286</td></tr>
  </tbody>
  
  
</table>
</div>

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

<img src="Analysis_files/figure-markdown_github/dwellings per person trend by city-1.png" width="864" />

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
  ggplot(aes(x = dpp_change, y = dpp, colour = grouping)) +
  geom_text(aes(label = area_name, fontface = face), size = 3) +
  geom_hline(yintercept = dpp_average) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(NA, 650)) +
  labs(x = "% change in 10 years", y = "Dwellings per person, latest year", title = "Dwellings per 1,000 people and change over 10 years (where available)", caption = "City names in italics, countries in bold") +
  scale_colour_okabeito() +
  theme(panel.grid.major.x = element_line(colour = "grey80"),
        panel.grid.major.y = element_line(colour = "grey90"),
        legend.position = "bottom",
        legend.title = element_blank())
```

<img src="Analysis_files/figure-markdown_github/dpp level and change scatter-1.png" width="768" />

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
  ggplot(aes(x = area_name, y = value, fill = variable)) +
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

<img src="Analysis_files/figure-markdown_github/dpp change decomposition - countries-1.png" width="768" />

At the country level, we can see that only three countries (Poland,
Japan and Portugal) saw a fall in their populations over the decade,
contributing to an increase in dwellings per person. And unsurprisingly,
no countries saw a decrease in their housing stock. Population growth
was highest in the Anglophone countries, which contrbuted to their low
or negative growth in dwellings per person. East Asian countries had low
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
  ggplot(aes(x = area_name, y = value, fill = variable)) +
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

<img src="Analysis_files/figure-markdown_github/dpp change decomposition - cities-1.png" width="768" />

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
  geom_smooth(se = F, span = 0.3) +
  scale_colour_okabeito() +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  coord_cartesian(ylim = c(200, 650)) + 
  labs(x = NULL, y = NULL, title = "Dwellings per 1,000 people by country grouping") +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey80"))
```

<img src="Analysis_files/figure-markdown_github/aggregate dwellings per person trend by country-1.png" width="672" />

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

<img src="Analysis_files/figure-markdown_github/dwellings per person trend by country with England focus-1.png" width="576" />

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

<img src="Analysis_files/figure-markdown_github/indexed change - countries-1.png" width="672" />

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

<img src="Analysis_files/figure-markdown_github/indexed change - cities-1.png" width="672" />

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

<img src="Analysis_files/figure-markdown_github/dwelling and GNP growth-1.png" width="672" />

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

<img src="Analysis_files/figure-markdown_github/annualised net dwellings per capita by country-1.png" width="864" />

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

<img src="Analysis_files/figure-markdown_github/net dwellings per capita by city-1.png" width="864" />

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

<img src="Analysis_files/figure-markdown_github/summary net additions chart by country-1.png" width="576" />

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

<img src="Analysis_files/figure-markdown_github/summary net additions chart with England focus-1.png" width="576" />

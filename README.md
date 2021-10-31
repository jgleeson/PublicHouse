## Openhouse
Openhouse bundles together data on the number of dwellings and people in cities and countries around the world. The initial [dataset](https://github.com/jgleeson/openhouse/blob/main/dataset.csv) was compiled by [James Gleeson](https://github.com/jgleeson), Housing Research and Analysis Manager at the Greater London Authority. 

### What's this for?
Openhouse has a very simple purpose - to facilitate international comparisons on trends in housing stock, including in comparison to trends in population. Many countries and cities publish data on their population and housing stock, but this data has not yet been brought together in a comprehensive and accessible dataset. One partial exception is the [OECD affordable housing database](https://www.oecd.org/housing/data/affordable-housing-database/housing-market.htm), but that covers only a limited number of country/year pairings and does not go down to city level (although it does also include several other variables that are not included here). 

### What years does the data cover?
Openhouse aims to include the latest available data, and an annual time series on each country and city extending as far back as data on dwellings is available (in many cases data on population is available further back, but that is not included here). The earliest data comes from the 1960s. Where annual data is not available, we use the next best thing (for example, decadal Census data). 

### What countries and cities are included?
The coverage of the dataset reflects an opportunistic approach to data gathering, which started with countries and cities that were considered internationally prominent and that made their data easily available in English (or in another language the project creator could understand), and then continued to add places on the basis of availability. A number of major countries are not covered due to a lack of comparable data: for example, China, Russia and developing countries (where the presence of substantial amounts of informal housing makes the formal dwelling count less meaningful).

In most countries just one city is included, either the capital or the largest city. One notable exception is Spain, where both Madrid and Barcelona are included.

### What details are included in the dataset?
To ensure transparency and to credit the creators of the data, the dataset includes a number of descriptive variables, as follows:

| Variable      | Description |
| ----------- | ----------- |
| year | Year the data refers to |
| ref_date | Reference date (often the start, middle or end of the calendar or financial year) |
| variable | Dwellings or population |
| value | Number of dwellings or people |
| area_name | The typically used name of the area |
| full_area_name | Formal name of the area |
| area_level | Country, city-region or city-state (e.g. Singapore) |
| country_name | Country name (useful for cities) |
| grouping | A somewhat ad-hoc grouping of countries |
| record_created | Date the record was created |
| record_updated | Date the record was last updated (same as creation date if no update) |
| source_org | Name of the organisation that produced the data |
| source_title | Title of the data source |
| source_pub_date | Publication date of the data source (if available) |
| frequency | Annual or multi-annual frequency (specified if regular intervals) |
| source_link | URL of the data source |
| notes | Miscellaneous notes on data definitions etc |

### How does this differ from other international datasets on housing supply?
As mentioned above, this dataset differs from the [OECD affordable housing database](https://www.oecd.org/housing/data/affordable-housing-database/housing-market.htm) as  that covers only a limited number of country/year pairings and does not go down to city level (although it does also include several other variables that are not included here). 

The other most comparable resource is [Sebastian Kohl's dataset](https://www.sebastiankohl.com/data) of annual housing construction over decades and across dozens of countries. His remarkable dataset has wider coverage in both space and time than Openhouse, because data on new construction is more widely reported and readily available than data on the housing stock. Another benefit of Sebastian's data is that it identifies the flow of *new* homes, whereas changes in the housing stock can be due to changes in the amount of demolition, the splitting of existing homes into a greater number of smaller homes, and other trends. The two datasets should be considered complementary, and there may be value in combining them for some analyses.  

### What are the limitations of the data?
Many! Looking at the housing side of the dataset, because we only collect the number of the dwellings we leave out important variables like dwelling size, type, condition, cost, occupancy and tenure. These are all important but they are also all difficult to compare across (and often within) national borders. The number of dwellings doesn't tell you everything you need to know about housing in a particular place, or anything close to it, but it tells you some things that are worth knowing. It's a starting point.

### Acknowledgements
This project would obviously have been impossible without the work of the many national and city statistical agencies that compiled the original data and made it available. Thanks also to [Jens von Bergmann](https://github.com/mountainMath) for advice on the Canadian data.

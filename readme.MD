# <img src="www/bird_pin_md.png" align="right" alt="" />
# Bird observations data from Poland
### Lucca de Farias Ramalho

In this app, you’ll be able to visualize bird species observations on
map.  
The data set contains observations only from from Poland.  

### <https://luccaramalho.shinyapps.io/biodiversity_observation/>

# Features

-   Visualize bird species observations on map with detailed information
    and image.
-   Visualize a timeline of bird species observations.
-   See the total count of observations for the selected specie.
-   Select between `Vernacular Name` or `Scientific Name` of bird
    species.
-   Select a `Date Range` of observations.

Source: `GBIF biodiversity-data`.

# Shiny app

### Data

------------------------------------------------------------------------

The data is imported at the beginning of the app. A reactive data set
`speciesReactiveValue()` is created to filter user’s input for
`Vernacular name` or `Scientific name`, and, `Date range`.

### Modal dialogue and random selected species on start

------------------------------------------------------------------------

Using a modal dialogue when the app starts is a great way of describing
what’s the application about and some of it’s features, improving the
user’s experience.  
A random observation is selected so the user can already have a
visualization loaded.

### Map visualization

------------------------------------------------------------------------

The reactive data set is loaded into a map visualization, using the
`leaflet` package.  
A pin marker shows the place where the observation was registered.  
A popup is shown after clicking on the marker, showing the
`Vernacular/Scientific name`, `Observation/Total count`, `Locality`,
`Date of observation` and an image, if there’s one available. If the
image is missing or does not exist, a local image `noimage.png` is
displayed.

### Total count value box

------------------------------------------------------------------------

A value box shows the `total count` for the selected bird specie, it’s
`Vernacular name` and the `Scientific name`. Instead of using an icon, a
custom image is shown.

### Timeline of observed species

------------------------------------------------------------------------

A timeline is shown with the total count by year of the selected
species, using `plotly` and `ggplot`. The date can be filtered at the
`Date range` input.

# Button actions

### Reset view

------------------------------------------------------------------------

The user can reset the view to default on the map.

### Reset data

------------------------------------------------------------------------

The user can reset data if he/she wishes to.

### Help/About

------------------------------------------------------------------------

Displays welcome modal dialog from the app.

# Extras

### Styling with CSS

------------------------------------------------------------------------

The app was styled usind the `birds.css` document. Two main colors are
shown in the app: `cadetblue` and `darkcyan`.  
The font-family used was `Lato`, imported from google-fonts api.

### Background image

------------------------------------------------------------------------

An image of birds flying are set as the background of the app, so the
user can already have a experience of and open environment for bird
observation, and, also, a cleaner web application.

### Custom marker

------------------------------------------------------------------------

A custom image is used as a marker pin to improve the user’s experience
and deliver a memorable brand.

# Data treatment

### 1. Data reading

------------------------------------------------------------------------

Read the biodiversity file. The file comes in `.tar.gz` format.

Set the filename

``` r
file_name <-"biodiversity-data.tar.gz"
```

List all the files contained in the file

``` r
untar(file_name, list = TRUE)
```

Extract data

``` r
untar(file_name)
```

Import `occurence.csv` file using `data.table` package with `UTF-8` as
encoding.

``` r
library(data.table)
occurence <- fread("biodiversity-data/occurence.csv",encoding='UTF-8')
```

## 2. Exploratory data analysis

------------------------------------------------------------------------

Number of rows

``` r
nrow(occurence)
```

Column names

``` r
names(occurence)
```

First 5 registers.

``` r
head(occurence)
```

Select only desired columns

``` r
library(dplyr)
occurence <- occurence %>% select("occurrenceID","vernacularName","scientificName","individualCount","longitudeDecimal","latitudeDecimal","continent","country","countryCode","stateProvince","eventDate","references","eventTime")
```

## 3. Data treatment

Most of the data treatment we’re done with `dplyr` pipe operators for a
cleaner code and faster execution.  

### Data filtering and manipulation

------------------------------------------------------------------------

Since the app wil display informations from bird species and from Poland
only, we must filter data frame’s column `countryCode` registers from
`Poland (PL)` and column `kingdom` only `Animalia` registers.  
Two new columns `year` and `month` are created based on `eventDate`, for
a better manipulation of the data.  
A new column `totalCount` is created, containing the sum from
`individualCount` for each species.

``` r
occurencePL <- occurence %>% filter(countryCode=="PL" & kingdom=="Animalia") %>% arrange(scientificName) %>% group_by(scientificName) %>% mutate(totalCount = sum(individualCount)) %>% mutate(month = format(eventDate, "%m"), year = format(eventDate, "%Y"))}
```

To filter only birds from the data frame, it is needed to import a bird
taxonomy data.  
Source: `https://ebird.org/science/use-ebird-data/the-ebird-taxonomy`.

``` r
birdTaxonomy <- fread("biodiversity-data/eBird_Taxonomy_v2021.csv", encoding = "UTF-8")
```

The information we’re looking for is inside the `FAMILY` column from the
`birdTaxonomy` dataframe. To recover the family name values, it is
needed to use regex and convert characters to upper case for later
pairing.

``` r
birdTaxonomy$FAMILY <- toupper(gsub(" \\(.*","",birdTaxonomy$FAMILY))
```

### Data cleaning

------------------------------------------------------------------------

Perform regex and set to upper case column `FAMILY`

``` r
birdTaxonomy$FAMILY <- toupper(gsub(" \\(.*","",birdTaxonomy$FAMILY))
```

Remove `_` character from `family` column in `occurencePL` dataframe.

``` r
occurencePL$family <- gsub("_"," ",occurencePL$family)
```

Select only last word from `family` column in `occurencePL` dataframe.

``` r
library(stringr)
occurencePL$family <- toupper(word(occurencePL$family,-1))
```

Subset data that contains only `birds`.

``` r
occurencePL <- subset(occurencePL, family %in% birdTaxonomy$FAMILY)
```

Import `multimedia` file from `biodiversity-data`.

``` r
multimedia <- fread("biodiversity-data/multimedia.csv",encoding='UTF-8')
```

Select only desired columns

``` r
multimedia <- multimedia %>% select(CoreId, accessURI, creator)
```

### Joining data

Left join `occurencePL` and `multimedia` by `id`=`CoreId`

``` r
occurencePL <- left_join(occurencePL, multimedia, by=c("id"="CoreId"))
```

## 3.1 Web scrapping

------------------------------------------------------------------------

Looking at the `accessURI` column, that contains an `URL` to the image
from the observation, we can see that there’s a lot of values missing
(`94%`).  

``` r
sum(is.na(occurencePL$accessURI)) / length(occurencePL$accessURI)
```

A function is created to recover the original observation register and
scrape through the HTML to recover the image file.

``` r
source("functions/image_scrapper.R")
occurencePL <- imageScrapper(occurencePL)
```

Percentage of `NA` values drops to `74%` after webscrapping.

``` r
sum(is.na(occurencePL$accessURI)) / length(occurencePL$accessURI)
```

## 4. Exporting treated data

------------------------------------------------------------------------

The data is exported after treatment.

``` r
write.csv(occurencePL,"biodiversity-data/occurencePL.csv")
```

Data prep
================

A lot in QGIS, so wanted to document steps. I really wanted to rasterize my VRI data and load it in to R, but this is proving really, really difficult. Apparently rasterization can be done entirely in R, but it's really really slow, so instead I'll try to find a more efficient way to extract polygon data for points in QGIS.

First step is to find all of my roost sites.

``` r
# Load some libraries.
library('tidyverse')
library('lubridate')
library('suncalc')

# Load in telemetry data.
tel.all <- read.csv('../data/processed/h_telem_all.csv', stringsAsFactors=F)

# Pull out the three sites I'm working with.
roam <- tel.all %>%
  filter(site %in% c('SKA', 'RLK', 'MTC')) %>%
  drop_na(lat)

# Do the datetime thing.
roam$date <- ymd(roam$date)
roam$datetime <- ymd_hms(roam$datetime, tz='America/Vancouver')

# Calculate sunrise and sunset times for each day.
roam <- getSunlightTimes(data=roam, keep=c('sunrise', 'sunset'), 
                        tz='America/Vancouver')

# Classify points as daytime or nighttime.
roam <- roam %>%
  mutate(dial=case_when(
    datetime < sunset & datetime > sunrise ~ 'day',
    TRUE ~ 'night'
  ))

# Pull out the nighttime points.
roam.night <- roam %>%
  filter(dial == 'night')

# Round each point to the nearest midnight.
roam.night <- roam.night %>%
  mutate(day=round_date(datetime, unit='day'))

# Average the locations.
centroids <- roam.night %>%
  group_by(site, day) %>%
  summarize(m.lat=mean(lat), m.lon=mean(lon))

# Save as a csv.
write.csv(centroids, '../data/processed/h_roam_roost.csv')
```

So I loaded the roost sites into QGIS and used `Join Attributes by Location` to annotate the points with data from the VRI (though I had to use `Fix Geometries` on the VRI data first).

Then I used `Regular points` to make a bunch of points spaced 500m apart across the whole study area (*why* does QGIS let me make random points inside a polygon but not regular ones?? This takes forever and makes waaaaay to many points.) And is 500m too close together?? IDK, I supposed I can always subsample them...

Then I clipped my grid of points to within the 100% MCPs for my three sites, and used the same spatial join to add attributes to these points (`available_utm.shp & .csv` in Processed). That gets me my available points.

To get unused points, I generated a 100m-radius buffer around each roost site. Then I took the full set of available points and took the difference, ie, those points which did not overlap the buffered roost sites.

Now check that everything looks ok...

``` r
# Check that I formatted everything right.

used <- read.csv('../data/processed/h_used_utm.csv')
available <- read.csv('../data/processed/available_utm.csv')
unused <- read.csv('../data/processed/unused_utm.csv')
```

Well, at a glance it looks fine!

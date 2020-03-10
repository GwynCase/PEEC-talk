Better Point Selection
================

Previously I selected roost sites by taking all the points for a single night and calculating an average location. But when I looked closer at the nighttime points I realized that sometimes the bird moved around in the middle of the night, meaning an average might not represent where it was actually sleeping. So I'll try a different way of selecting points.

I also realized I accidentally grabbed both the male *and* female points from Mt. Currie, when I only wanted the male.

``` r
# Load some libraries.
library('tidyverse')
library('lubridate')
library('suncalc')

# Load in telemetry data.
tel.all <- read.csv('../h_data/processed/h_telem_all.csv', stringsAsFactors=F)

# Pull out the three sites I'm working with.
roam <- tel.all %>%
  filter(id %in% c('HAR05', 'HAR04', 'HAR09')) %>%
  drop_na(lat)

# Do the datetime thing.
roam$date <- ymd(roam$date)
roam$datetime <- ymd_hms(roam$datetime, tz='America/Vancouver')
```

That gives me the dataframe all nicely set up. One study selected the point closest to midnight, which makes sense, but that's a bit complicated. So instead I'll grab the last point of the day, which should be pretty close to midnight.

``` r
roost <- roam %>% 
  group_by(site, date) %>%
  filter(datetime == max(datetime))
```

I thought I wouldn't have to split day and night points with this methods, but it looks like there are some days with no night points, meaning the max datetime is during daylight.

``` r
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

# And then try this again.
roost <- roam.night %>% 
  group_by(site, date) %>%
  filter(datetime == max(datetime))
```

OK, now to see how close this actually gets me to midnight. Because R does stupid things with time, I can't really use midnight because `00:00:00` just collapses down to the date, with no time. So I'm using `23:59:59` as close enough.

``` r
roost <- roost %>%
  mutate(midnight=ymd_hms(date, tz='America/Vancouver') + 
           dseconds(43199)) %>%
  mutate(difs=difftime(midnight, datetime, unit='hours'))

roost %>% ungroup() %>% summarize(
  mean=mean(difs),
  min=min(difs),
  max=max(difs)
)
```

    ## # A tibble: 1 x 3
    ##   mean           min              max           
    ##   <drtn>         <drtn>           <drtn>        
    ## 1 1.376265 hours 0.01638889 hours 22.99972 hours

So most of these are really close to midnight, but there are few major outliers, which is really frustrating because it means the day/night split didn't work properly. So I've definitely got some daytime points mixed in, and I may have some nighttime points missing. Which means it's aaaaall the way back to the beginning.

``` r
# Pull out the three sites I'm working with.
roam <- tel.all %>%
  filter(id %in% c('HAR05', 'HAR04', 'HAR09')) %>%
  drop_na(lat)

# Pick the last observation of the day.
roost <- roam %>% 
  group_by(site, date) %>%
  filter(datetime == max(datetime))

# Do math.
roost <- roost %>% mutate(midnight=paste(date, '23:59:59')) %>%
  mutate(midnight=ymd_hms(midnight, tz='America/Vancouver')) %>%
  mutate(datetime=ymd_hms(datetime, tz='America/Vancouver')) %>%
  mutate(difs=difftime(midnight, datetime, unit='hours'))

# Check.
roost %>% ungroup() %>% summarize(
  mean=mean(difs),
  min=min(difs),
  max=max(difs)
)
```

    ## # A tibble: 1 x 3
    ##   mean           min              max           
    ##   <drtn>         <drtn>           <drtn>        
    ## 1 1.137943 hours 0.01638889 hours 22.99972 hours

Actually about the same. Well, that's a bit reassuring, I guess. I'll select those points that are less than four hours away from midnight.

``` r
roost <- roost %>% filter(difs < 4)

# And save it.
write.csv(roost, '../h_data/processed/used_2.csv')
```

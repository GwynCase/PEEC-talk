Model checking
================

So following on the model and model resuts I produced last time, I need to check on a few things.

-   Dig more into how the foraging HSI compares to the nesting HSI
-   Check that missing NAs are randomly distributed
-   Look at more than just AICs

``` r
# Load some libraries.
library('ggplot2')
library('tidyverse')
library('lme4')

# Read in the points.
points <- read.csv('../data/interim/points.csv', stringsAsFactors=FALSE)

# Do all the transformations as last time.
f.points <- points %>%
  filter(n_hab >= 0) %>%
  filter(!cover=='') %>% 
  mutate(density=case_when(
  density=='RZ' ~ 'NL',
  density=='UR' ~ 'NL',
  TRUE ~ density
)) %>%
  filter(cover %in% c('TC', 'TM', 'TB'))
```

That was probably actually a lot of extra steps... but it gives me all the forest points, with the NAs included. Which looks like this:

``` r
map(f.points, ~sum(is.na(.)))
```

    ## $xcoord
    ## [1] 0
    ## 
    ## $ycoord
    ## [1] 0
    ## 
    ## $site
    ## [1] 0
    ## 
    ## $case
    ## [1] 0
    ## 
    ## $cover
    ## [1] 0
    ## 
    ## $density
    ## [1] 0
    ## 
    ## $canopy.closure
    ## [1] 0
    ## 
    ## $basal.area
    ## [1] 32
    ## 
    ## $live.stems
    ## [1] 0
    ## 
    ## $v.comp
    ## [1] 81
    ## 
    ## $dom.sp.per
    ## [1] 0
    ## 
    ## $age
    ## [1] 0
    ## 
    ## $height
    ## [1] 0
    ## 
    ## $f_hab
    ## [1] 0
    ## 
    ## $n_hab
    ## [1] 0

So I still have `NAs` for basal area, and vertical complexity. I gave Little's MCAR test a shot, though from some of the chatter online it's not actually a very good test.

``` r
library('BaylorEdPsych')
# Requires mvnmle to be installed

missing <- f.points %>% select(7:15) %>% 
  LittleMCAR()
```

    ## this could take a while

``` r
missing$chi.square
```

    ## [1] 364.1557

``` r
missing$p.value
```

    ## [1] 0

``` r
missing$amount.missing
```

    ##                 canopy.closure  basal.area live.stems     v.comp dom.sp.per age
    ## Number Missing               0 32.00000000          0 81.0000000          0   0
    ## Percent Missing              0  0.04295302          0  0.1087248          0   0
    ##                 height f_hab n_hab
    ## Number Missing       0     0     0
    ## Percent Missing      0     0     0

The math for Little's test it well beyond me, but if it's doing what I think it's doing than it's no surprise it finds the missing values to be non-random. Obviously more values are missing from `v.comp` (81) than `age` (0). But that's not really what I care about, is it? I'm worried whether removing observations with NAs will bias the overall distribution of the variables.

I think what I really need are somee f-tests and t-tests, but for that I need normal data... which I don't have. So... something nonparametric?

``` r
# Make the second data set, without NAs
n.points <- f.points %>%
  drop_na()

# Just a quick run...
wilcox.test(f.points$age, n.points$age)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  f.points$age and n.points$age
    ## W = 229620, p-value = 0.02007
    ## alternative hypothesis: true location shift is not equal to 0

I always struggle with p-values. Here, the null hypothesis is that the two ages are the same. A large p-value "accepts" the null hypothesis, and a small p-value rejects it. Here the p-value of 0.02 is slightly smaller than 0.05, so I have to reject the null and say the ages really are different.

Damn.

How about the other variables?

``` r
wilcox.test(f.points$n_hab, n.points$n_hab)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  f.points$n_hab and n.points$n_hab
    ## W = 233900, p-value = 0.06806
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(f.points$canopy.closure, n.points$canopy.closure)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  f.points$canopy.closure and n.points$canopy.closure
    ## W = 230280, p-value = 0.02417
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(f.points$basal.area, n.points$basal.area)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  f.points$basal.area and n.points$basal.area
    ## W = 228490, p-value = 0.2645
    ## alternative hypothesis: true location shift is not equal to 0

So nesting habitat isn't different, basal area isn't different, and v.comp isn't different, but canopy closure is different.

Well, it makes sense that vertical complexity would be the same, since that's the limiting factor. What if I drop vertical complexity from my analysis?

``` r
f.points <- f.points %>%
  dplyr::select(-v.comp)

n.points <- f.points %>%
  drop_na()

wilcox.test(f.points$n_hab, n.points$n_hab)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  f.points$n_hab and n.points$n_hab
    ## W = 257880, p-value = 0.3205
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(f.points$canopy.closure, n.points$canopy.closure)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  f.points$canopy.closure and n.points$canopy.closure
    ## W = 255040, p-value = 0.1862
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(f.points$basal.area, n.points$basal.area)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  f.points$basal.area and n.points$basal.area
    ## W = 254180, p-value = 1
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(f.points$age, n.points$age)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  f.points$age and n.points$age
    ## W = 254190, p-value = 0.1558
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(f.points$age, n.points$height)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  f.points$age and n.points$height
    ## W = 480680, p-value < 2.2e-16
    ## alternative hypothesis: true location shift is not equal to 0

So removing vertical complexity altogether mostly fixed things (though height is still weird, for some reason). How do my models look when I re-run them this way?

``` r
# "Boost" models.
b.basal <- glmer(case ~ n_hab + basal.area + (1|site), data=n.points, family=binomial(link='logit'))

b.canopy <- glmer(case ~ n_hab + canopy.closure + (1|site), data=n.points, family=binomial(link='logit'))

# "Solo" models.
s.basal <- glmer(case ~ basal.area + (1|site), data=n.points, family=binomial(link='logit'))

s.canopy <- glmer(case ~ canopy.closure + (1|site), data=n.points, family=binomial(link='logit'))

s.age <- glmer(case ~ age + (1|site), data=f.points, family=binomial(link='logit'))

# And of course habitat models.
n.hab <- glmer(case ~ n_hab + (1|site), data=n.points, family=binomial(link='logit'))

f.hab <- glmer(case ~ f_hab + (1|site), data=n.points, family=binomial(link='logit'))

# And one novel one.
can.age <- glmer(case ~ age + canopy.closure + (1|site), data=n.points, family=binomial(link='logit'))

# Check what pops out.
aic <- AIC(b.basal, b.canopy, s.basal, s.canopy, s.age, n.hab, f.hab, can.age)

aic %>% rownames_to_column() %>%
  arrange(AIC)
```

    ##    rowname df      AIC
    ## 1 b.canopy  4 561.2787
    ## 2 s.canopy  3 564.2833
    ## 3    n.hab  3 565.4237
    ## 4  can.age  4 565.7867
    ## 5  b.basal  4 566.4585
    ## 6    f.hab  3 566.6714
    ## 7  s.basal  3 570.1225
    ## 8    s.age  3 586.8616

Ok, so this result is a bit more like what I expected. When the habitat model is assisted by canopy closure it performs best. Canopy closure still performs better than the nesting model alone, though. And it occurred to me that basal area is meaningless, since it's an area for the entire polygon, and that means nothing without polygon area/live stems.

So!

(Also, I just realized I didn't scale my variables this time around and yet I didn't get any errors. So that's nice.)

``` r
# Scale basal area by number of trees.
n.points <- n.points %>% mutate(m.basal=basal.area/live.stems)

# New models.
b.mbase <- glmer(case ~ n_hab + m.basal + (1|site), data=n.points, family=binomial(link='logit'))

s.mbase <- glmer(case ~ m.basal + (1|site), data=n.points, family=binomial(link='logit'))

fb.canopy <- glmer(case ~ f_hab + canopy.closure + (1|site), data=n.points, family=binomial(link='logit'))

aic <- AIC(b.basal, b.canopy, s.basal, s.canopy, s.age, n.hab, f.hab, can.age, b.mbase, s.mbase, fb.canopy)

aic %>% rownames_to_column() %>%
  arrange(AIC)
```

    ##      rowname df      AIC
    ## 1   b.canopy  4 561.2787
    ## 2  fb.canopy  4 562.2697
    ## 3   s.canopy  3 564.2833
    ## 4      n.hab  3 565.4237
    ## 5    can.age  4 565.7867
    ## 6    b.basal  4 566.4585
    ## 7      f.hab  3 566.6714
    ## 8    b.mbase  4 567.4220
    ## 9    s.basal  3 570.1225
    ## 10   s.mbase  3 572.7912
    ## 11     s.age  3 586.8616

Hey! I was right! The foraging model does work better in combination with canopy closure than the nesting model. Probably because of the distance from edge.

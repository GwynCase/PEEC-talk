Cross Validation
================

Ok, to get a more absolute idea of how good my models are, I want to calculate som R2s.

``` r
# Load some libraries.
library('tidyverse')
library('lme4')
library('AICcmodavg')

# Read in the points.
points <- read.csv('../h_data/interim/points.csv', stringsAsFactors=FALSE)

# Get the final data set.
n.points <- points %>%
  filter(n_hab >= 0) %>%
  filter(cover %in% c('TC', 'TM', 'TB')) %>%
  select(-v.comp) %>%
  drop_na()

# Build the models.
b.canopy <- glmer(case ~ n_hab + canopy.closure + (1|site), data=n.points, family=binomial(link='logit'))

fb.canopy <- glmer(case ~ f_hab + canopy.closure + (1|site), data=n.points, family=binomial(link='logit'))

s.canopy <- glmer(case ~ canopy.closure + (1|site), data=n.points, family=binomial(link='logit'))

n.hab <- glmer(case ~ n_hab + (1|site), data=n.points, family=binomial(link='logit'))

f.hab <- glmer(case ~ f_hab + (1|site), data=n.points, family=binomial(link='logit'))

can.age <- glmer(case ~ age + canopy.closure + (1|site), data=n.points, family=binomial(link='logit'))

s.age <- glmer(case ~ age + (1|site), data=n.points, family=binomial(link='logit'))

site <- glmer(case ~ 1 + (1|site), data=n.points, family=binomial(link='logit'))

models <- c(b.canopy, fb.canopy, s.canopy, n.hab, f.hab, can.age, s.age, site)

modnames <- c('canopy + nest hab', 'canopy + f hab', 'canopy alone', 'nest hab alone', 'f hab alone', 'canopy + age', 'age alone', 'site')

aictab(models, modnames=modnames)
```

    ## 
    ## Model selection based on AICc:
    ## 
    ##                   K   AICc Delta_AICc AICcWt Cum.Wt      LL
    ## canopy + nest hab 4 561.34       0.00   0.47   0.47 -276.64
    ## canopy + f hab    4 562.33       0.99   0.28   0.75 -277.13
    ## canopy alone      3 564.32       2.98   0.11   0.86 -279.14
    ## nest hab alone    3 565.46       4.12   0.06   0.92 -279.71
    ## canopy + age      4 565.84       4.51   0.05   0.96 -278.89
    ## f hab alone       3 566.71       5.37   0.03   1.00 -280.34
    ## site              2 571.91      10.58   0.00   1.00 -283.95
    ## age alone         3 573.58      12.25   0.00   1.00 -283.77

And then calculate the R^2. `R2m` being the marginal R2, the amount of variance explained by the fixed factors alone, and `R2c` being the conditional R2, the amount of variance explained by both fixed and random factors.

``` r
library('MuMIn')

r.squaredGLMM(b.canopy)
```

    ##                    R2m        R2c
    ## theoretical 0.05762594 0.12844303
    ## delta       0.02379519 0.05303733

``` r
r.squaredGLMM(n.hab)
```

    ##                    R2m        R2c
    ## theoretical 0.02964582 0.08335153
    ## delta       0.01188064 0.03340335

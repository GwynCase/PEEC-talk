First model
================

So from the variables I looked at previously, some starting points:

-   case ~ nesting habitat
    -   Basically, NOGO roost in the same kind of places that they nest.
-   case ~ canopy closure + basal area + density
    -   Kitchen sink of promising variables BUT canopy and basal are highly correlated (0.8 correlation coefficient)
-   case ~ canopy closure + vertical complexity + density
    -   Alternate kitchen sink
-   case ~ nesting habitat + canopy closure
    -   Assumes the HSI is good but could be improved by an addition.

... and for all of them include site?

And I *think* use `glmer`.

``` r
# Load some libraries.
library('ggplot2')
library('tidyverse')
library('lme4')

points <- read.csv('../data/interim/points.csv', stringsAsFactors=FALSE)

n.hab <- glmer(case ~ n_hab + (1|site), data=points,
      family=binomial(link='logit'))

sink.corr <- glmer(case ~ canopy.closure + basal.area + density + (1|site), data=points, family=binomial(link='logit'))
## Gives me weird error about 'Model is nearly unidentifiable'??

sink.alt <- glmer(case ~ canopy.closure + v.comp + density + (1|site), data=points, family=binomial(link='logit'))
## Same error.

n.plus <- glmer(case ~ n_hab + canopy.closure + (1|site), data=points, family=binomial(link='logit'))
```

None of those variables taken individually throws an error...

``` r
canopy <- glmer(case ~ canopy.closure + (1|site), data=points, family=binomial(link='logit'))

density <- glmer(case ~ density + (1|site), data=points, family=binomial(link='logit'))

basal <- glmer(case ~ basal.area + (1|site), data=points, family=binomial(link='logit'))

vertical <- glmer(case ~ v.comp + (1|site), data=points, family=binomial(link='logit'))
```

So if I do a stupid thing and ignore the errors, I get this:

``` r
AIC(n.hab, sink.corr, sink.alt, n.plus)
```

    ##           df      AIC
    ## n.hab      3 598.8784
    ## sink.corr  7 571.2528
    ## sink.alt   7 547.4080
    ## n.plus     4 577.8058

``` r
summary(n.hab)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: case ~ n_hab + (1 | site)
    ##    Data: points
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    598.9    613.1   -296.4    592.9      833 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.5508 -0.3920 -0.3307 -0.2447  4.5461 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  site   (Intercept) 0.2122   0.4607  
    ## Number of obs: 836, groups:  site, 3
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.58334    0.33002  -7.828 4.97e-15 ***
    ## n_hab        0.34007    0.08717   3.901 9.57e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr)
    ## n_hab -0.459

``` r
summary(sink.corr)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: case ~ canopy.closure + basal.area + density + (1 | site)
    ##    Data: points
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    571.3    603.6   -278.6    557.3      740 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.7161 -0.4078 -0.3496 -0.2317 11.5264 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  site   (Intercept) 0.2821   0.5311  
    ## Number of obs: 747, groups:  site, 3
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)     -2.144460   0.685548  -3.128  0.00176 **
    ## canopy.closure   0.010942   0.010770   1.016  0.30964   
    ## basal.area      -0.003122   0.009476  -0.329  0.74183   
    ## densityLA      -17.867704 136.837847  -0.131  0.89611   
    ## densityOP       -0.531290   0.277487  -1.915  0.05554 . 
    ## densitySP       -2.265629   1.107706  -2.045  0.04082 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) cnpy.c basl.r dnstLA dnstOP
    ## canopy.clsr -0.614                            
    ## basal.area  -0.144 -0.577                     
    ## densityLA   -0.001  0.001  0.000              
    ## densityOP   -0.570  0.398  0.063  0.001       
    ## densitySP   -0.368  0.315  0.014  0.001  0.291
    ## convergence code: 0
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
summary(sink.alt)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: case ~ canopy.closure + v.comp + density + (1 | site)
    ##    Data: points
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    547.4    579.2   -266.7    533.4      690 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.5937 -0.4254 -0.3574 -0.2449  3.1322 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  site   (Intercept) 0.2791   0.5283  
    ## Number of obs: 697, groups:  site, 3
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)     -2.102032   0.892019  -2.356   0.0184 *
    ## canopy.closure   0.008472   0.008958   0.946   0.3443  
    ## v.comp          -0.009447   0.188305  -0.050   0.9600  
    ## densityLA      -14.210812 138.592569  -0.103   0.9183  
    ## densityOP       -0.488070   0.280518  -1.740   0.0819 .
    ## densitySP      -17.940345  50.946959  -0.352   0.7247  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) cnpy.c v.comp dnstLA dnstOP
    ## canopy.clsr -0.719                            
    ## v.comp      -0.645  0.094                     
    ## densityLA   -0.005  0.004  0.003              
    ## densityOP   -0.302  0.478 -0.166  0.003       
    ## densitySP    0.000  0.000  0.000  0.300  0.000
    ## convergence code: 0
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
summary(n.plus)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: case ~ n_hab + canopy.closure + (1 | site)
    ##    Data: points
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    577.8    596.5   -284.9    569.8      780 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6830 -0.4372 -0.3289 -0.2164  6.4608 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  site   (Intercept) 0.3073   0.5544  
    ## Number of obs: 784, groups:  site, 3
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -3.709334   0.532654  -6.964 3.31e-12 ***
    ## n_hab           0.199915   0.096860   2.064 0.039022 *  
    ## canopy.closure  0.024621   0.006946   3.545 0.000393 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) n_hab 
    ## n_hab       -0.135       
    ## canopy.clsr -0.698 -0.240

Now, `AIC()` also throws an error warning me that there are a different number of observations for each model. Which might be part of the "model unrecognizable" problem. I might need to sort through my data to select some lowest common denomenator of points.

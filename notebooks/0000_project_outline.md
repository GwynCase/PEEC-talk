Project Outline
================

**2020-02-05**

I have TWO WEEKS to put together a talk for PEEC 2020. Yes, it's only a 3-minute talk but still.

Plan A
------

Existing HSI & nest area buffers focus on identifying & protecting NOGO nests (which are important) BUT other behaviours are also important and may not be either predicted or protected by HSI & buffers.

Roosting is one of the other important behaviours. Here I use roosting behaviour to assess whether HSI & buffers are effective at protecting this daily activity.

Also, here are some other things that may do a better job of predicting roosts sites/here is how big a buffer really needs to be to protect roosts.

Outcome One: HSI & buffers are terrible at predicting & protecting roosting sites. Imagine how bad they must be for foraging sites... which will come next.

Outcome Two: HSI & buffers are great at predicting & protecting roosting sites. It remains to be seen whether they will be as good for foraging sites... which will come next.

Plan B
------

Animals will reuse places that are important to them and/or are of higher quality. Nests in high-quality habitat should be used more often than nests in low-quality habitats.

Roosting is one kind of habitat that could be reused. Here I use roost sites to examine reuse and return time.

And how does this extend to foraging habitat... future directions.

Action Items
------------

-   Get HSI "heatmap" for nesting model
-   Calculate roost site centroids
-   Pull land cover variables for each roost site
-   Generate random points within 100% MCP
-   Pull land cover variables for each random point
-   Draw 2-ha buffer around nests
-   Measure how well buffers cover roost sites
-   Compare used vs. available for variables
-   Test predictive power of HSI

Work in QGIS
------------

### Data prep

To get the land cover variables for each roost site, I loaded a .csv of the roost site centroids (calculated in R) into QGIS. I also loaded a shapefile of VRI polygons.

### Variable extraction

`Toolbox > Add polygon attributes to points > attribute of interest`

Repeat this for each attribute `BCLCS_LV_4`, `BASAL_AREA`, `LIVE STEMS`, `CR_CLOSURE`, `VERT_COMPL`, and `PROJ_AGE_1`

I learned the hard way these two layers have to be in the same projection for this to work, otherwise you get a lot of empty fields. Then:

`Vector > Data Management Tools > Join Attributes by Location`

and clean up duplicate columns. This is very repetitive and there must be a better way to do it.

### Random points

`Vector > Research Tools > Random Points Inside Polygons`

Weirdly, this results in no coordinates, so I had to open go to `Attribute Table > Field calculator` and create a new field called `xcoord` and populate it with `$x` (and then do the same for the y coord).

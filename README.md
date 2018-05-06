# grower-portal
An iteration of a farmer-facing Shiny app to report back research measurements on biomass collection for leaf litter decomposition bags

Live demo: https://brianwdavis.shinyapps.io/Biomass/
- *Try out "kirk" or "cole"*

----
The .R files needed to build the app are in this repository, but API keys have been censored, so they won't actually run.

This app demonstrates: 
 - pulling data from Google Sheets (where it's stored in somewhat untidy formats), 
 - munging the sheets, and 
 - synthesizing a report for each grower-collaborator. 
 
The `leaflet` plugin shows a map preview to help the farmer identify which sites correspond to our internal research IDs. `ggplot2` is used to summarise the distribution of data across sites within the farmer's region. The code is `tidyverse`-flavored **R**, and `shiny` then handles the generation of the necessary **HTML** and **JS**.

While the intended end user is the farmer, the research technical team also refers to this app when conducting outreach.

## Next steps:
Add tabs for additional metrics from this study: soil water status and crop yield.

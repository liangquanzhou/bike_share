pckg <- function(x) {
  if (!require(x, character.only = T)) {
    install.packages(x, dependencies = T)
    library(x, character.only = T)
  } else library(x, character.only = T)
}

pckg("devtools")
pckg("tidyverse")
pckg("lubridate")
pckg("magrittr")
pckg("leaflet")
pckg("gridExtra")
pckg("ggmap")
pckg("ggrepel")
pckg("tibbletime")
pckg("leaps")
pckg("car")
pckg("timeDate")
pckg("xaringan")
pckg("DT")
pckg("Kmisc")
pckg("knitr")
pckg("rmarkdown")
source("src/Regression.R")
rmarkdown::render("report/report.Rmd")
xaringan::infinite_moon_reader("docs/index.Rmd")
# to stop server use: ?servr::daemon_stop
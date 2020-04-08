library(tidyverse)
library(scales)

source("process_qcew.R")
source("process_cps.R")
source("process_ui.R")
source("create_tables.R")

# publish draft tables
rmarkdown::render("index.Rmd")
draft_server <- Sys.getenv("DRAFT_SERVER")
system(paste0("~/.local/bin/aws s3 cp index.html ", draft_server, "ui_claims_health_insurance/"))

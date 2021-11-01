source("scripts/visualize.R")
source("scripts/functions.R")

if (!packageCompliance()) {
  shinyjs::alert("Package issue, please address and restart.")
  stopApp()
}
dsn <- read_rds(file.path("data", "DSN.RDS"))
CT_SQL_name <- read_rds(file.path("data", "SQLnameCT.RDS"))
counts <- read_rds(file.path("data", "counts.RDS"))
freezerCapacity <- read_rds(file.path("data", "freezerCapacity.RDS"))
containerRatios <- read_rds(file.path("data", "containerRatios.RDS"))
basketCounts <- read_rds(file.path("data", "basketCounts.RDS"))
freezers <- read_rds(file.path("data", "freezers.RDS"))
n_freezers <- dim(freezers)[1]
frz_list <- sort(freezers$FreezerPhysName)
freezer_counts <- getFreezerCounts()
spaceDat <- spaceUsed(freezer_counts,
                      basketCounts, 
                      containerRatios)

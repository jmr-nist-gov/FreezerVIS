
packageCompliance <- function() {
  success = FALSE
  if (require(odbc) == FALSE) {
    install.packages("odbc")
    library(odbc)
  }
  if (require(DBI) == FALSE) {
    install.packages("DBI")
    library(DBI)
  }
  if (require(tidyverse) == FALSE) {
    install.packages("tidyverse")
    suppressPackageStartupMessages(library(tidyverse))
  }
  if (require(shiny) == FALSE) {
    install.packages("shiny")
    library(shiny)
  }
  if (require(shinyjs) == FALSE) {
    install.packages("shinyjs")
    library(shinyjs)
  }
  if (require(plotly) == FALSE) {
    install.packages("plotly")
    library(plotly)
  }
  success = TRUE
  return(success)
}

validConnection <- function(dsn) {
  tmp <- try(dbConnect(odbc(), dsn))
  if (class(tmp) == "try-error") {
    return(FALSE)
  } else {
    con <- dbConnect(odbc(), dsn)
    return(TRUE)
  }
}

cleanUp <- function(aliquotCounts, freezerSections, freezers) {
  if ("FK_FreezerSectID" %in% names(aliquotCounts)) {
    aliquotCounts <- aliquotCounts %>%
      left_join(freezerSections %>%
                  select("PK_FreezerSectID", "FK_FreezerPhysID"),
                by = c("FK_FreezerSectID" = "PK_FreezerSectID")) %>%
      left_join(freezers %>%
                  select("PK_FreezerPhysID", "FreezerPhysName"), 
                by = c("FK_FreezerPhysID" = "PK_FreezerPhysID"))
  } else {
    aliquotCounts <- aliquotCounts %>%
      left_join(freezerSections %>%
                  select("PK_FreezerSectID", "FK_FreezerPhysID"),
                by = c("FK_FREEZERSECTID" = "PK_FreezerSectID")) %>%
      left_join(freezers %>%
                  select("PK_FreezerPhysID", "FreezerPhysName"), 
                by = c("FK_FreezerPhysID" = "PK_FreezerPhysID"))
  }
  attr(aliquotCounts, "asof") <- Sys.time()
  return(aliquotCounts)
}

refreshData <- function(session, output, dsn) {
  con <- dbConnect(odbc(), dsn)
  freezers <- dbReadTable(con, "FreezerPhysical")
  freezerSections <- dbReadTable(con, "FreezerSection")
  aliquotCounts <- dbGetQuery(con,
                              "SELECT FK_FreezerSectID,
                              Container_Type,
                              Position1,
                              Position2,
                              Position3,
                              Position4
                              FROM Aliquots
                              WHERE NOT FK_FreezerSectID = 0 AND NOT Position1 = ''")
  dbDisconnect(con)
  rm(con)
  aliquotCounts <- cleanUp(aliquotCounts, freezerSections, freezers)
  write_rds(aliquotCounts, path = "data/counts.RDS", compress = "none")
}

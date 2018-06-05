# Generate current counts of aliquots by container type present in the NIST Marine Environmental Specimen Bank
# --------
# Author: Jared M. Ragland
# Date: 2018-05-16
# --------
# Results in a data frame suitable for plotting in ggplot or plotly
# Automatically produces a ranked-order stacked column chart
# Counts are grouped by Freezer Section and Container Type

# Ensure package compliance
cat("Starting Freezer Visual Information System (Freezer VIS)...\n")
cat(paste("Started at", Sys.time(), "\n"))
cat("Checking package compliance...\n")
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
cat(paste("Package compliance passsed at", Sys.time(), ".\n"))

cat("Connecting to 4D driver...\n")
# Clear up any prior existing connection called "con"
if (exists("con")) {
  if (dbIsValid(con) == TRUE) dbDisconnect(con)
  rm(con)
}

# Connect to the 64-bit 4D driver for Freezerworks (requires separate driver installation)
con <- dbConnect(odbc(), "Freezerworks64")
cat(paste("Connected to Freezerworks at", Sys.time(), ".\n"))

# Execute queries and then disconnect/clean up
cat("Downloading most recent data...this may take a moment.\n")
freezers <- dbReadTable(con, "FreezerPhysical")
cat("\t-- Freezer information loaded.\n")
freezerSections <- dbReadTable(con, "FreezerSection")
cat("\t-- Freezer Section information loaded.\n")
aliquotCounts <- dbGetQuery(con, 
                           "SELECT FK_FreezerSectID,
                    Container_Type, 
                    Tissue_Type, 
                    Date_In, 
                    Date_Out, 
                    Position1, 
                    Position2, 
                    Position3, 
                    Position4 
                    FROM Aliquots 
                    WHERE NOT FK_FreezerSectID = 0 AND NOT Position1 = ''")
cat("\t-- Aliquot Count source loaded.\n")
cat("Success! Disconnecting from Freezerworks.\n")
dbDisconnect(con)
rm(con)
cat(paste("Disconnected successfully at", Sys.time(), ".\n"))
cat("Wrapping up the aliquot counts...\n")
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
cat("Success!\n\n")
cat("Freezerworks Visual Information System is now ready to be used.\n")
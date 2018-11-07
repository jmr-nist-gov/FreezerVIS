configureFreezerVIS <- function(DSN){
  cat("Configuring the Freezer Visual Information System...\n")
  cat("----------------------------------------------------\n")
  cat("Loading libraries and scripts...\n")
  source("scripts/packageCompliance.R")
  source("scripts/functions.R")
  write_rds(DSN, "data/DSN.RDS")
  
  # Catch prior configuration through this mechanism
  if (file.exists("data/ACFVIS.RDS")) {
    config_history <- read_rds("data/ACFVIS.RDS")
    review_config <- askYesNo("FreezerVIS was previously configured on this system. Would you like to review that configuration? Select 'No' to continue with a fresh configuration.")
    if (review_config) {
      View(config_history)
      stop("To start the configuration again,  again to restart the process and select no")
    } else {
      if (!askYesNo("This will overwrite your current configuration. Are you certain?")) {
        stop("Configuration aborted.")
      }
    }
  }

  # Placeholders for user responses
  getFreezers <- FALSE
  edited_freezers <- FALSE
  getContainers <- FALSE
  edited_CTRs <- FALSE
  
  # Begin generating data sources by trying to connect to Freezerworks and create placeholder tables.
  refresh <- askYesNo("Generate data sources directly from Freezerworks?")
  if (refresh) {
    cat("Validating connection to Freezerworks\n")
    tmp <- try(dbConnect(odbc(), DSN))
    if (class(tmp) == "try-error") {
      stop("Unable to connect to Freezerworks at ", DSN, " at this time. Please address and try again.\n")
    } else {
      cat("Successful connection available...\n")
    }
    cat("Connecting to Freezerworks at", DSN, "and refreshing basic tables. This will take a moment...\n")
    refreshData(DSN)
  
    getFreezers <- askYesNo("Generate placeholder table for Freezer Capacities?")
    cat("\nFreezer Capacities:\n")
    if (getFreezers){
      freezers <- read_rds("data/freezers.RDS")
      freezer_capacity <- freezers %>%
        select(Description) %>%
        unique() %>%
        rename(DESCRIPTION = Description) %>%
        mutate(Size = NA,
               Capacity = 1,
               DESCRIPTION = as.character(DESCRIPTION)) %>%
        filter(DESCRIPTION != "")
      cat("Freezer descriptions are now available.\n")
      cat("This list includes all freezer types as listed in Freezerworks, including the demo and any virtual freezers you may have.\n")
      edited_freezers <- askYesNo("Edit this table now? For small lists, this is easier. For a large number of freezers it is easier to import separately.")
      if (edited_freezers){
        cat("Edit the 'Capacity' column to reflect the total capacity of each freezer type and then close the window. Do not change the 'DESCRIPTION' column.\n")
        freezer_capacity <- edit(freezer_capacity)
      } else {
        cat("If importing data later, ensure freezer descriptions match exactly with those generated here.\n")
      }
      write_rds(freezer_capacity, "data/freezerCapacity.RDS")
    } else {
      cat("You did not generate a placeholder for freezer capacities directly from Freezerworks.")
    }
    
    # Create placeholder for container types
    getContainers <- askYesNo("Pull container types?")
    cat("\nContainer Types Ratios:\n")
    if (getContainers){
      containers <- read_rds("data/counts.RDS") %>%
        distinct(CONTAINER_TYPE) %>%
        filter(!is.na(CONTAINER_TYPE)) %>%
        arrange(CONTAINER_TYPE) %>%
        mutate(CT_Ratio = 1, 
               CONTAINER_TYPE = as.character(CONTAINER_TYPE))
      cat("This list includes all container types as listed in Freezerworks.\n")
      edited_CTRs <- askYesNo("Edit this table now? For small lists, this is easier. For a large number of container types it is easier to import separately.")
      if (edited_CTRs) {
        cat("Edit the 'CT_Ratio' column to reflect your container types and then close the window. Do not change the 'CONTAINER_TYPE' column.\n")
        containers <- edit(containers)
      } else {
        cat("If importing data later, ensure container types match exactly with those generated here.\n")
      }
      write_rds(containers, "data/containerRatios.RDS")
    } else {
      cat("You did not generate a placeholder for container type ratios directly from Freezerworks.")
    }
    
    # Create basket containers holding space from list of freezers
    setupBaskets()
    
    if (askYesNo("Review steps and data sources at this time?")) {
      view_all <- askYesNo("Open all data tables automatically for viewing?")
      cat("\nFreezerVIS setup results:\n")
      cat("\t1. FreezerVIS successfully loaded and ran setup.\n")
      counts <<- read_rds("data/counts.RDS")
      if (view_all) View(counts)
      freezers <<- read_rds("data/freezers.RDS")
      if (view_all) View(freezers)
      out_message <- "'counts', 'freezers'"
      cat("\t2. Data were pulled directly from Freezerworks.\n")
      if (getFreezers) {
        freezer_capacity <<- read_rds("data/freezerCapacity.RDS")
        if (view_all) View(freezer_capacity)
        out_message <- paste0(out_message, ", 'freezer_capacity'")
        if (edited_freezers) {
          cat("\t3. You edited freezer capacities directly during setup.\n")
        } else {
          cat("\t3. You did not edit freezer capacities directly during setup. Please do so prior to running FreezerVIS.\n")
        }
      } else {
        cat("\t3. You did not generate freezer capacities during setup. Please provide this via import and save to 'data/freezerCapacity.RDS'.\n")
      }
      if (getContainers) {
        container_ratios <<- read_rds("data/containerRatios.RDS")
        if (view_all) View(container_ratios)
        out_message <- paste0(out_message, ", 'container_ratios'")
        if (edited_CTRs) {
          cat("\t4. You edited container type ratios directly during setup.\n")
        } else  {
          cat("\t4. You did not edit container type ratios directly during setup. Please do so prior to running FreezerVIS.\n")
        }
        cat("\t\t\tRemove any container types from this data source for which a space estimate is unavailable.\n")
      } else {
        cat("\t4. You did not generate container ratios during setup. Please provide this via import and save to 'data/containerRatios.RDS'.\n")
      }
      basket_counts <<- read_rds("data/basketCounts.RDS")
      if (view_all) View(basket_counts)
      cat("\t5. A placeholder table for basket counts (default maximum of 12 baskets with zero used per freezer) was created for application functionality.\n")
      out_message <- paste0(out_message, ", and 'basket_counts'")
      cat("\nData objects", out_message, "are available within R for your review.\n")
    }
  } else {
    cat("\t2. You did not pull data directly from Freezerworks for setup. Please provide properly formatted .RDS data sources prior to running FreezerVIS.\n")
    cat("\t\t\tThe easiest way to do this is to import a comma-separated-value file via the command 'saveRDS(read_csv('path_to_file'), 'save_pathname.RDS')' or by importing through the 'Files' or 'Environment' panes in RStudio and then saving through via 'saveRDS(object, 'save_pathname.RDS')'.\n")
    cat("\t\t\tYou will need objects in the 'data' directory named:\n")
    cat("\t\t\t\t1. 'freezerCapacity.RDS' where freezer descriptions match exactly with Freezerworks entries;\n")
    cat("\t\t\t\t2. 'containerRatios.RDS' where container types match exactly with Freezerworks entries; and\n")
    cat("\t\t\t\t3. 'basketCounts.RDS' where freezer names match exactly with Freezerworks entries.\n")
    cat("\t\t\tExamples of these data sources are provided in the FreezerVIS distribution downloaded from GitHub.\n")
  }
  cat("\nAutoconfiguration is complete. There may be idiosyncracies in your installation of Freezerworks that do not directly match the architecture of FreezerVIS.\n")
  cat("\nFreezerVIS is distributed without copyright under the NIST Software Disclaimer (https://www.nist.gov/disclaimer) and is explicitly provided 'as-is'.\n")
  cat("Last updated 4 October 2018 at https://github.com/jmr-nist.gov/FreezerVIS.\n")
  cat("\nContact the developer: Jared M. Ragland - jared.ragland@nist.gov\n")
  
  # Create wrap up and generate the configuration history
  config_history <- data.frame(
    "Step" = c("Connection to Freezerworks",
               "Tables Refreshed",
               "Freezer Capacity",
               "Container Type Ratios",
               "Basket Counts"),
    "Configuration" = c(ifelse(refresh, "Successful", "Untested"),
                        ifelse(refresh, "Successful", "Untested"),
                        if (getFreezers) {
                          if(edited_freezers) {
                            "Generated and edited."
                          } else {
                            "Generated but not edited."
                          }
                        } else {
                          "Manual upload selected"
                        },
                        if (getContainers) {
                          if (edited_CTRs) {
                            "Generated and edited."
                          } else {
                            "Generated but not edited."
                          }
                        } else {
                          "Manual upload selected."
                        },
                        ifelse(refresh, "Successful", "Manual upload selected.")
    )
  )
  write_rds(config_history, "data/ACFVIS.RDS")
}

setupBaskets <- function() {
  if (file.exists("data/freezers.RDS")) {
    basketCounts <- read_rds("data/freezers.RDS") %>%
      select(PK_FreezerPhysID, FreezerPhysName) %>%
      mutate(useOutputID = gsub(" ", "", FreezerPhysName),
             useInputID = paste0(useOutputID, "slide"),
             maxvalue = 12,
             value = 0,
             FreezerPhysName = as.character(FreezerPhysName)) %>%
      rename('freezerphysname' = FreezerPhysName)
    write_rds(basketCounts, "data/basketCounts.RDS")
  } else {
    stop("Cannot find file 'data/freezers/RDS', run 'configureFreezerVIS(DSN)' first.")
  }
}
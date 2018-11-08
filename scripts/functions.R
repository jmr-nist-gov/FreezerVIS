# Ensures all necessary packages are installed and loaded.
# Requires internet connection.
packageCompliance <- function() {
  success = FALSE
  source("scripts/packageCompliance.R")
  success = TRUE
  return(success)
}

# Checks that a valid connection can be made to the Freezerworks server via ODBC
# Requires the driver defined by dsn to be established.
validConnection <- function(dsn) {
  tmp <- try(dbConnect(odbc(), dsn))
  if (class(tmp) == "try-error") {
    return(FALSE)
  } else {
    con <- dbConnect(odbc(), dsn)
    return(TRUE)
  }
}

# Properly combines multiple data frames into the target format/structure.
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

# Refreshes all necessary data to generate aliquot counts.
# Also caches files necessary in case connectivity fails.
refreshData <- function(dsn, CT_SQL_name) {
  con <- dbConnect(odbc(), dsn)
  freezers <- dbReadTable(con, "FreezerPhysical")
  freezerSections <- dbReadTable(con, "FreezerSection")
  query <- paste0("SELECT FK_FreezerSectID, ",
                  CT_SQL_name,
                  ", Position1, ",
                  "Position2, ",
                  "Position3, ",
                  "Position4 ",
                  "FROM Aliquots ",
                  "WHERE NOT FK_FreezerSectID = 0 AND NOT Position1 = ''")
  aliquotCounts <- dbGetQuery(con, query)
  dbDisconnect(con)
  rm(con)
  aliquotCounts <- cleanUp(aliquotCounts, freezerSections, freezers)
  write_rds(aliquotCounts, path = "data/counts.RDS", compress = "none")
  write_rds(freezers, path = "data/freezers.RDS", compress = "none")
}

# Get freezer counts. This mostly supports repository-level visualization
getFreezerCounts <- function(.counts = counts, .freezers = freezers, .freezerCapacity = freezerCapacity, .containerRatios = containerRatios) {
  freezer_counts <- .counts %>%
    group_by(FreezerPhysName) %>%
    count(CONTAINER_TYPE) %>%
    left_join(.freezers %>% 
                select(FreezerPhysName, Description)) %>%
    left_join(.freezerCapacity %>%
                select(-Size),
              by = c("Description" = "DESCRIPTION")) %>%
    left_join(.containerRatios) %>%
    mutate(uses_capacity = round((n*CT_Ratio/Capacity), 4)) %>%
    select(FreezerPhysName, CONTAINER_TYPE, n, CT_Ratio, Capacity, uses_capacity) %>%
    ungroup()
  # freezer_counts$FreezerPhysName <- as.factor(freezer_counts$FreezerPhysName)
  freezer_counts$CONTAINER_TYPE <- as.factor(freezer_counts$CONTAINER_TYPE)
  names(freezer_counts) <- tolower(names(freezer_counts))
  return(freezer_counts)
}

# Calculate used freezer space from getFreezerCounts().
# Returns a series of useful metrics as a list.
# Argument 'freezer_counts' should be the output of 'getFreezerCounts()'
#   1. Total estimated space at the repository level
#     a.  All Freezers------------------------------------------------------------------(.$total_est_all)
#     b.  Only freezers used in other visualizations------------------------------------(.$total_est_same)
#   2. Correct sort orders for freezers in the plots
#     a.  All freezers------------------------------------------------------------------(.$freezer_plot_order_all)
#     b.  Only freezers used in other visualizations------------------------------------(.$freezer_plot_order_same)
#   3. Which containers do not have a space ratio---------------------------------------(.$na_containers)
#   4. Counts of bad containers by freezers in the plots
#     a.  All freezers------------------------------------------------------------------(.$bad_containers_all)
#     b.  Only freezers used in other visualizations------------------------------------(.$bad_containers_same)
#   5. Correct sort order for bad containers--------------------------------------------(.$bad_freezer_order)
#   6. The main data frame for use in repository-level visualizations to be fed into
#       vis_bank()----------------------------------------------------------------------(.$freezer_space_used)
#   7. If meanCR = TRUE, returns the mean container type ratio from containerRatios
#       which is needed if meanCR = TRUE------------------------------------------------(.$meanCR)
spaceUsed <- function(.freezer_counts = freezer_counts, 
                      .basketCounts = basketCounts, 
                      .containerRatios = containerRatios, 
                      meanCR = FALSE) {
  
  freezer_counts <- .freezer_counts
  basketCounts <- .basketCounts
  containerRatios <- .containerRatios
  
  # Get vector of bad containers
  na_containers <- c(NA, 
                     levels(freezer_counts$container_type)[-which(levels(freezer_counts$container_type) %in% containerRatios$CONTAINER_TYPE)])
  
  freezer_space_used <- freezer_counts
  
  if (meanCR) {
    if (is.null(containerRatios)) {
      stop("Argument 'containerRatios' is necessary if 'meanCR = TRUE'")
    }
    meanCR <- mean(unique(containerRatios$CT_Ratio))
    freezer_space_used <- freezer_space_used %>%
      mutate(uses_capacity = ifelse(is.na(uses_capacity), n*meanCR/capacity, uses_capacity))
  }
  
  freezer_space_used <- freezer_space_used %>%
    group_by(freezerphysname) %>% 
    summarise(used_capacity = sum(uses_capacity, na.rm=T)) %>%
    full_join(basketCounts) %>%
    mutate(used_capacity = ifelse(is.na(used_capacity), 0, used_capacity),
           used_capacity = used_capacity + (value/maxvalue),
           freezerphysname = as.factor(freezerphysname))
  
  # Get plot orders
  freezer_plot_order_all <- freezer_space_used %>%
    mutate(freezerphysname = fct_reorder(freezerphysname, used_capacity)) %>%
    pull(freezerphysname) %>%
    levels()
  
  freezer_plot_order_same <- freezer_space_used %>%
    filter(used_capacity > 0) %>%
    mutate(freezerphysname = fct_reorder(freezerphysname, used_capacity)) %>%
    filter(freezerphysname %in% unique(freezer_space_used$freezerphysname)) %>%
    pull(freezerphysname) %>%
    droplevels() %>%
    levels()
  
  # Estimate used space for the entire bank
  total_est_all <- sum(freezer_space_used$used_capacity, na.rm = TRUE) / length(unique(freezer_space_used$freezerphysname))
  total_est_all <- round(total_est_all * 100, 2)
  tmp <- freezer_space_used %>%
    filter(used_capacity > 0) %>%
    droplevels()
  total_est_same <- sum(tmp$used_capacity) / length(unique(tmp$freezerphysname))
  total_est_same <- round(total_est_same * 100, 2)

  # Bad container type counts and freezer order
  bad_containers <- freezer_counts %>%
    filter(freezerphysname %in% levels(freezer_space_used$freezerphysname),
           container_type %in% na_containers) %>%
    group_by(freezerphysname, container_type) %>%
    summarise(nn = sum(n)) %>%
    ungroup()
  
  bad_freezer_order <- bad_containers %>%
    group_by(freezerphysname) %>%
    summarise(nnn = sum(nn)) %>%
    arrange(nnn) %>%
    pull(freezerphysname) %>%
    as.character()
  
  bad_containers$freezerphysname <- factor(bad_containers$freezerphysname,
                                           levels = bad_freezer_order)
  
  # Include all freezers
  bad_containers_all <- bad_containers %>%
    filter(freezerphysname %in% freezer_plot_order_all)
  
  # Include same freezers as other plots
  bad_containers_same <- bad_containers %>%
    filter(freezerphysname %in% freezer_plot_order_same) %>%
    droplevels()
  
  return(list(total_est_all = total_est_all,
              total_est_same = total_est_same,
              freezer_plot_order_all = freezer_plot_order_all,
              freezer_plot_order_same = freezer_plot_order_same,
              na_containers = na_containers,
              bad_containers_all = bad_containers_all,
              bad_containers_same = bad_containers_same,
              bad_freezer_order = bad_freezer_order,
              freezer_space_used = freezer_space_used))
}

# Internal function to prepare data for doBoxCount()
prepDat <- function(dat, freezer) {
  out <- dat %>% 
    filter(FreezerPhysName == freezer)
  out$CONTAINER_TYPE <- as.character(out$CONTAINER_TYPE)
  out$CONTAINER_TYPE[is.na(out$CONTAINER_TYPE)] <- "Not Recorded"
  out$CONTAINER_TYPE <- factor(out$CONTAINER_TYPE)
  out <- droplevels(out)
  return(out)
}

# Internal function to prepare data for doBoxCount()
# Runs on output of prepDat()
prepBoxCount <- function(dat) {
  boxes <- dat %>% 
    filter(POSITION3 != "") %>%
    mutate(POSITION1 = paste("Rack", POSITION1))
  boxsort <- sort(unique(boxes$POSITION2))
  tubes <- dat %>% 
    filter(POSITION3 == "") %>%
    mutate(POSITION3 = POSITION2,
           POSITION2 = POSITION1,
           POSITION1 = "Tube/Other")
  tubesort <- sort(unique(as.numeric(tubes$POSITION2)))
  out <- rbind(boxes, tubes)
  out$POSITION2 <- factor(out$POSITION2,
                          levels = unique(c(tubesort, boxsort)))
  out$POSITION1 <- as.factor(out$POSITION1)
  return(out)
}

# Counts aliquots within each position collection (boxes, racks, tubes, etc.)
doBoxCount <- function(dat, freezer) {
  dat <- prepDat(dat, freezer)
  dat <- prepBoxCount(dat)
  dat <- dat %>%
    group_by(POSITION1, POSITION2, CONTAINER_TYPE) %>%
    summarise(nSAMPLES = n_distinct(POSITION3))
  return(dat)
}

# Adds additional factors for empty sets to ensure consistent color factors in ggplotly
addDummyContainers <- function(setFull, setSub) {
  # Need to make certain to capture all factors for ggplotly to work properly
  fullLevels <- levels(setFull$Container)
  subLevels <- levels(droplevels(setSub$Container))
  for (diff in setdiff(fullLevels, subLevels)) {
    dummy_name = paste0('dummy_', diff)
    setSub[dummy_name, ] <- setSub[1, ]
    setSub[dummy_name, ]$Container <- diff
    setSub[dummy_name, ]$Count <- 0
  }
  return(setSub)
}

# Generates the freezer-specific visualizations of aliquots within containers.
# Does not support separation of baskets from boxes; baskets are considered boxes.
# Returns two ggplot objects if both tubes and racks exist in the freezer.
# Returns NULL if tubes or racks do not exist.
vizBoxCount <- function(dat, freezer) {
  vizBoth <- doBoxCount(dat, freezer) %>%
    rename("Box" = POSITION2,
           "Container" = CONTAINER_TYPE,
           "Count" = nSAMPLES) %>%
    ungroup()
  vizRack <- vizBoth %>% 
    filter(POSITION1 != "Tube/Other")
  if (nrow(vizRack) > 0) {
    vizRack <- addDummyContainers(vizBoth, vizRack)
    vizRack <- vizRack %>%
      ggplot(aes(x = Box,
                 y = Count,
                 fill = Container)) +
      scale_fill_discrete(drop = FALSE) +
      geom_bar(stat = 'identity') +
      facet_wrap(~POSITION1,
                 scales = "free") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=90,
                                       vjust=0.5,
                                       hjust=1),
            axis.title.y = element_text(vjust = 0)) +
      labs(x = "Box ID",
           y = "Aliquot Count",
           fill = "Container Type",
           title = paste0(freezer, " as of ", attr(dat, "asof")))
  } else {
    vizRack = NULL
  }
  vizTubes <- vizBoth %>% 
    filter(POSITION1 == "Tube/Other") %>%
    rename("Tube" = Box)
  if (nrow(vizTubes) > 0) {
    vizTubes <- addDummyContainers(vizBoth, vizTubes)
    vizTubes <- vizTubes %>%
      ggplot(aes(x = Tube,
                 y = Count,
                 fill = Container)) +
      geom_bar(stat = 'identity',
               show.legend = FALSE) +
      theme_minimal() +
      labs(x = "Container ID",
           y = "Aliquot Count",
           fill = "Container Type",
           title = paste0(freezer, " as of ", attr(dat, "asof"))) +
      theme(axis.text.x = element_text(angle=90,
                                       vjust=0.5,
                                       hjust=1))
  } else {
    vizTubes = NULL
  }
  return(list("Racks" = vizRack,
              "Tubes" = vizTubes))
}

# Creates the freezer-level visualization using vizBoxCount - better to use outside the app.
#   1. Set inApp = FALSE to use directly.
#   2. Set usePlotly = FALSE to keep ggplot format.
#   3. Set saveMe = TRUE to save to an environment variable.
#   4. Set saveFile = TRUE to save the result as an RDS.
# FreezerVIS sets inApp = TRUE and uses other option defaults.
vizFreezer <- function(counts, freezer, inApp=FALSE, usePlotly=TRUE, saveMe=FALSE, saveFile=FALSE) {
  if (freezer %in% counts$FreezerPhysName) {
    out <- vizBoxCount(counts, freezer)
    if (usePlotly) {
      if (is.null(out[[1]]) & is.null(out[[2]])){
        out <- paste("No data found for", freezer)
      } else if (is.null(out[[1]])) {
        out <- ggplotly(out[[2]])
      } else if (is.null(out[[2]])) {
        out <- ggplotly(out[[1]])
      } else {
        out <- subplot(out[[1]],
                       style(out[[2]], showlegend = FALSE),
                       nrows = 2,
                       heights = c(0.7, 0.25),
                       titleX = TRUE,
                       titleY = TRUE)
      }
    }
    if (!is.character(out[1])) {
      out <- out %>%
        layout(margin = list(b = 40, l = 50, t = 30),
               legend = list(y=0.97))
    }
    if (inApp) {
      return(out)
    } else {
      print(out)
    }
    if (saveMe) {
      return(out)
    }
    if (saveFile) {
      write_rds(out, gsub(" ", "_", paste0("NIST_MESB_", freezer, "_visual_saved_", Sys.time(), ".RDS")))
    }
  } else {
    if (inApp) {
      return(NULL)
    } else {
      cat(paste0("Freezer '", freezer, "' not found."))
    }
  }
}


# Creates the freezer-level visualization directly using plotly to better space subplots.
# Better to use inside the app but less flexible for reporting purposes.
vizFreezerPlotly <- function(counts, freezer) {
  asof <- attributes(counts)$asof
  counts <- counts %>%
    filter(FreezerPhysName == freezer) %>%
    mutate(CONTAINER_TYPE = ifelse(is.na(CONTAINER_TYPE), 
                                   "Not Recorded", 
                                   CONTAINER_TYPE))
  frzRacks <- counts %>%
    filter(POSITION3 != '') %>%
    group_by(POSITION1, CONTAINER_TYPE) %>%
    count(POSITION2) %>%
    ungroup()
  rackList <- unique(frzRacks$POSITION1)
  frzTubes <- counts %>%
    filter(POSITION3 == '') %>%
    group_by(CONTAINER_TYPE) %>%
    count(POSITION1) %>%
    ungroup() %>%
    mutate(POSITION1 = as.numeric(POSITION1)) %>%
    arrange(POSITION1)
  container_levels <- sort(
    unique(
      c(unique(frzRacks$CONTAINER_TYPE),
        unique(frzTubes$CONTAINER_TYPE))
    )
  )
  toAdd <- which(container_levels %in% frzTubes$CONTAINER_TYPE == FALSE)
  if (length(toAdd) > 0) {
    for (container_type in container_levels[toAdd]) {
      frzTubes <- rbind(frzTubes, 
                        data.frame(POSITION1 = unique(frzTubes$POSITION1)[1],
                                   CONTAINER_TYPE = container_type,
                                   n = 0))
    }
  }
  if (is.factor(frzTubes$CONTAINER_TYPE)) {
    frzTubes$CONTAINER_TYPE <- as.character(frzTubes$CONTAINER_TYPE)
  }
  frzTubes <- frzTubes %>%
    arrange(CONTAINER_TYPE) %>%
    mutate(CONTAINER_TYPE = factor(CONTAINER_TYPE,
                                    levels = container_levels))

  toAdd <- 0
  
  plotlys <- list()
  if (dim(frzRacks)[1] > 0) {
    plotlys$racks <- lapply(rackList, function(rack) {
      t <- frzRacks %>% 
        group_by(POSITION1) %>%
        filter(POSITION1 == rack) %>%
        as.data.frame()
      if (is.factor(t$CONTAINER_TYPE)) {
        t$CONTAINER_TYPE <- as.character(t$CONTAINER_TYPE)
      }
      if (rack == rackList[1]) {
        showYesNo <- TRUE
      } else {
        showYesNo <- FALSE
      }
      toAdd <- which(container_levels %in% t$CONTAINER_TYPE == FALSE)
      if (length(toAdd) > 0) {
        for (container_type in container_levels[toAdd]) {
          t <- rbind(t, data.frame(POSITION1 = rack,
                                   CONTAINER_TYPE = container_type,
                                   POSITION2 = unique(t$POSITION2)[1],
                                   n = 0))
        }
      }
      t <- t %>% 
        arrange(CONTAINER_TYPE) %>%
        mutate(CONTAINER_TYPE = factor(CONTAINER_TYPE, 
                                       levels = container_levels),
               hovertext = paste0(
                 if (grepl("Upright", freezer)) {
                   paste0("</br>Shelf ", rack,
                          "</br>Basket ", POSITION2)
                 } else {
                   if (is.na(as.numeric(POSITION2))) {
                     paste0("</br>Rack ", rack,
                            "</br>Box ", POSITION2)
                   } else {
                     paste0("</br>Pie ", rack,
                            "</br>Basket ", POSITION2)
                   }
                 },
                 "</br>", CONTAINER_TYPE,
                 "</br>Count ", n)
        )
      p <- t %>% 
        plot_ly(x = ~POSITION2,
                y = ~n,
                color = ~CONTAINER_TYPE,
                legendgroup = ~CONTAINER_TYPE,
                text = ~hovertext,
                hoverinfo = 'text',
                showlegend = showYesNo) %>%
        add_bars(orientation = 'v') %>%
        layout(barmode = 'stack',
               annotations = list(text = ifelse(grepl("Upright", freezer) | grepl("Mechanical", freezer),
                                                paste("Shelf", rack),
                                                ifelse(length(which(is.na(as.numeric(t$POSITION2)))) > 0,
                                                       paste("Rack", rack),
                                                       paste("Pie", rack))
                                                ),
                                  xref = 'paper',
                                  yref = 'paper',
                                  xanchor = 'left',
                                  yanchor = 'bottom',
                                  align = 'center',
                                  x = 0, 
                                  y = 1, 
                                  showarrow = FALSE,
                                  font = list(size = 12,
                                              color = 'black')
               ),
               xaxis = list(type = 'category',
                            tickangle = 0),
               yaxis = list(range = c(0, max(t$n)))
        )
      return(p)
    })
  }
  if (dim(frzTubes)[1] > 0) {
    plotlys$tubes <- plot_ly(data = frzTubes,
                             x = ~POSITION1,
                             y = ~n,
                             color = ~CONTAINER_TYPE,
                             legendgroup = ~CONTAINER_TYPE,
                             text = ~paste0("</br>Tube: ", POSITION1,
                                            "</br>", CONTAINER_TYPE,
                                            "</br>Count: ", n),
                             hoverinfo = 'text',
                             showlegend = ifelse(length(rackList) == 0, TRUE, FALSE)) %>%
      add_bars(orientation = 'v') %>%
      layout(barmode = 'stack',
             annotations = list(text = "Tubes",
                                xref = 'paper',
                                yref = 'paper',
                                xanchor = 'left',
                                yanchor = 'bottom',
                                align = 'center',
                                x = 0,
                                y = 1,
                                showarrow = FALSE,
                                font = list(size = 14,
                                            color = 'black')
             ),
             xaxis = list(type = 'category',
                          range = list(sort(as.numeric(frzTubes$POSITION1))),
                          tickangle = 90
             ),
             yaxis = list(range = c(0, max(frzTubes$n))
             )
      )
  }
  plot_rows <- ceiling(sqrt(length(plotlys$racks))-1)
  plot_rows <- ifelse(plot_rows == 0, 1, plot_rows)
  if (!is.null(plotlys$racks) & !is.null(plotlys$tubes)) {
    plot_heights <- c(plot_rows/(plot_rows+1), 1/(plot_rows+1))
    p <- subplot(
      subplot(plotlys$racks, 
              nrows = plot_rows, 
              margin = c(0.01, 0.01, 0.04, 0.04)
      ), 
      plotlys$tubes, 
      nrows = 2, 
      heights = plot_heights,
      margin = c(0.01, 0.01, 0.06, 0.01)
    )
  } else if (is.null(plotlys$racks) & is.null(plotlys$tubes)) {
    p <- NULL
  } else if (is.null(plotlys$racks)) {
    p <- subplot(
      plotlys$tubes,
      nrows = 1,
      margin = 0.03
    )
  } else if (is.null(plotlys$tubes)) {
    p <- subplot(
      plotlys$racks,
      nrows = plot_rows,
      margin = 0.03
    )
  }
  if (!is.null(p)) {
    p <- p %>%
      layout(title = paste0("<b>", freezer, "</b>",
                            " as of ",
                            asof),
             margin = list(t = 75, pad = 0)
      )
    return(p)
  }
}

# Generates repository-level faceted pie charts, one per freezer
# Argument 'dat' should be the output of spaceUsed()
# Set all = TRUE to visualize every freezer recorded in Freezerworks
# Set all = FALSE to visualize only those freezers used in other visualizations.
vizUsedFacets <- function(dat, showAll = FALSE, useMeanCR = FALSE, n_columns) {
  if (showAll) {
    p <- dat$freezer_space_used %>%
      filter(!is.na(used_capacity))
  } else {
    p <- dat$freezer_space_used %>%
      filter(used_capacity > 0) %>%
      droplevels()
  }
  p <- p %>% 
    ggplot(aes(y = used_capacity*100,
               x = factor(1),
               fill = used_capacity*100)) +
    geom_col(width = 1) +
    theme_bw(base_size = 18) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = 'bottom') +
    facet_wrap(~paste0(gsub("Upright Freezer", "Upright", freezerphysname), "\n", (100*round(used_capacity, 3)), "%"), 
               ncol = n_columns) +
    scale_fill_gradientn(colours = c("darkgreen", "blue", "orange", "darkred", "darkred"),
                         values = c(0, 0.5, 0.75, 0.9, 1)) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(from = 0, to = 100, by = 10)) +
    coord_polar("y")
  if (useMeanCR) {
    labs_caption = paste("Mean space ratio of", round(dat$meanCR, 2), "used for")
  } else {
    labs_caption = paste("Space utilization unable to be estimated for")
  }
  if (showAll) {
    p <- p  + labs(title = paste0("Estimated Space Utilization: ", dat$total_est_all, "%"),
                   subtitle = paste("NIST Biorepository, Charleston, SC as of", Sys.Date(),
                                    "\nAll freezers are shown."),
                   caption = paste(labs_caption,
                                   length(unique(dat$bad_containers_all$container_type)),
                                   "container types.\nThis includes", 
                                   comma(sum(dat$bad_containers_all$nn)),
                                   "of",
                                   comma(sum(freezer_counts %>% 
                                               ungroup() %>% 
                                               filter(freezerphysname %in% dat$freezer_plot_order_all) %>%
                                               pull(n))),
                                   "aliquots."),
                   fill = "Used Capacity (%)")
  } else {
    p <- p  + labs(title = paste0("Estimated Space Utilization: ", dat$total_est_same, "%"),
                   subtitle = paste("NIST Biorepository, Charleston, SC as of", Sys.Date(),
                                    "\nOnly freezers with good space estimates are shown."),
                   caption = paste(labs_caption,
                                   length(unique(dat$bad_containers_same$container_type)),
                                   "container types.\nThis includes", 
                                   comma(sum(dat$bad_containers_same$nn)), 
                                   "of",
                                   comma(sum(freezer_counts %>% 
                                               ungroup() %>% 
                                               filter(freezerphysname %in% dat$freezer_plot_order_same) %>% 
                                               pull(n))),
                                   "aliquots."),
                   fill = "Used Capacity (%)")
  }
  return(p)
}

# Generates the repository-level race track chart.
# Argument 'dat' should be the output of spaceUsed()
# Set all = TRUE to visualize every freezer recorded in Freezerworks
# Set all = FALSE to visualize only those freezers used in other visualizations.
vizUsedRaceTrack <- function(dat, showAll = FALSE, useMeanCR = FALSE) {
  if (showAll) {
    p <- dat$freezer_space_used %>%
      filter(!is.na(used_capacity))
  } else {
    p <- dat$freezer_space_used %>%
      filter(used_capacity > 0)
  }
  p <- p %>% 
    mutate(freezerphysname = fct_reorder(freezerphysname, used_capacity)) %>%
    ggplot(aes(x = freezerphysname,
               y = used_capacity*100)) +
    geom_col(aes(fill = used_capacity*100)) +
    geom_label(aes(label = paste0(gsub("Upright Freezer", "Upright", freezerphysname), " (", round(used_capacity*100, 0), "%)")), 
               y = 0,
               size = 3,
               label.size = NA, 
               label.padding = unit(0.1, "lines"),
               color = "black",
               fill = "white",
               alpha = 0.8,
               lwd = NA,
               hjust = 1,
               vjust = 0.3) +
    theme_classic(base_size = 18) + 
    theme(axis.line = element_blank(), 
          legend.position = "bottom",
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()) +
    coord_polar("y", start = 0) +
    scale_y_continuous(limits = c(0, 100), labels = c(0, 25, 50, 75, 100)) + 
    scale_fill_gradientn(colours = c("darkgreen", "blue", "orange", "red", "darkred"),
                         values = c(0, 0.5, 0.75, 0.9, 1))
  if (useMeanCR) {
    labs_caption = paste("Mean space ratio of", round(dat$meanCR, 2), "used for")
  } else {
    labs_caption = paste("Unable to estimate space for")
  }
  if (showAll) {
    p <- p  + labs(title = paste0("Estimated Space Utilization: ", dat$total_est_all, "%"),
                   subtitle = paste("NIST Biorepository, Charleston, SC as of", Sys.Date(),
                                    "\nAll freezers are shown."),
                   caption = paste(labs_caption,
                                   length(unique(dat$bad_containers_all$container_type)),
                                   "container types.\nThis includes", 
                                   comma(sum(dat$bad_containers_all$nn)),
                                   "of",
                                   comma(sum(freezer_counts %>% 
                                               ungroup() %>% 
                                               filter(freezerphysname %in% dat$freezer_plot_order_all) %>%
                                               pull(n))),
                                   "aliquots."),
                   fill = "Used Capacity (%)")
  } else {
    p <- p  + labs(title = paste0("Estimated Space Utilization: ", dat$total_est_same, "%"),
                   subtitle = paste("NIST Biorepository, Charleston, SC as of", Sys.Date(),
                                    "\nOnly freezers with good space estimations are shown."),
                   caption = paste(labs_caption,
                                   length(unique(dat$bad_containers_same$container_type)),
                                   "container types.\nThis includes", 
                                   comma(sum(dat$bad_containers_same$nn)), 
                                   "of",
                                   comma(sum(freezer_counts %>% 
                                               ungroup() %>% 
                                               filter(freezerphysname %in% dat$freezer_plot_order_same) %>% 
                                               pull(n))),
                                   "aliquots."),
                   fill = "Used Capacity (%)")
  }
  return(p)
}

vizBadContainers <- function(dat, showAll = FALSE, usePlotly = FALSE) {
  if (showAll) {
    dat <- dat$bad_containers_all
  } else {
    dat <- dat$bad_containers_same
  }
  dat <- dat %>%
    mutate(container_type = ifelse(is.na(container_type),
                                   "Not Recorded",
                                   as.character(container_type)))
  p_labels <- dat %>% 
    group_by(freezerphysname) %>% 
    summarise(n_tot = sum(nn))
  if (usePlotly) {
    count_containers <- length(unique(dat$container_type))
    count_aliquots <- sum(p_labels$n_tot)
    p <- dat %>%
      plot_ly(x = ~nn,
              y = ~freezerphysname,
              color = ~container_type,
              text = ~paste0("</br>", freezerphysname,
                             "</br>", container_type,
                             "</br>Count:", nn),
              hoverinfo = 'text'
              ) %>%
      add_bars() %>%
      add_annotations(data = p_labels,
                      x = ~n_tot+25,
                      y = ~freezerphysname,
                      text = ~n_tot,
                      showarrow = FALSE) %>%
      layout(barmode = 'stack',
             margin = list(l = 170, 
                           r = 20, 
                           t = 100, 
                           b = 20),
             yaxis = list(title = ''),
             xaxis = list(title = ''),
             legend = list(x = 0.8,
                           y = 0.45),
             title = paste('<b>Unable to Estimate Space Used for',
                           count_containers,
                           'Container Types</b>\n',
                           'Labels are the total number of aliquots by freezer.\n',
                           'This represents',
                           count_aliquots,
                           'aliquots.')
      )
  } else {
    p <- dat %>%
      ggplot(aes(x = freezerphysname,
                 y = nn,
                 fill = container_type)) +
      geom_col(position = 'stack') +
      geom_text(dat = p_labels,
                aes(y = n_tot,
                    label = comma(n_tot),
                    fill = NA),
                colour = 'black',
                hjust = 0,
                nudge_y = 10) +
      scale_y_continuous(label = comma,
                         limits = c(0, max(p_labels$n_tot)*1.1)) +
      theme_classic(base_size = 18) +
      theme(axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = c(0.75, 0.33)) +
      coord_flip() +
      labs(title = paste('Unable to Estimate Space Used by', length(unique(dat$container_type)), 'Container Types'),
           subtitle = paste0('Labels are the total number of aliquots by freezer.\n',
                             'This represents ', comma(sum(p_labels$n_tot)), ' aliquots.'),
           x = 'Freezer',
           fill = 'Container Type')
  }
  return(p)
}

# Convenience function to visualize the bank.
vizSpace <- function(vizType = "facets", showAll = FALSE, meanCR = FALSE, n_columns = 5, usePlotly = FALSE) {
  if (vizType == "facets") {
    vizUsedFacets(spaceDat, showAll, meanCR, n_columns)
  } else if (vizType == "track") {
    vizUsedRaceTrack(spaceDat, showAll, meanCR)
  } else if (vizType == 'bad') {
    vizBadContainers(spaceDat, showAll, usePlotly)
  } else {
    stop("Argument 'type' not recognized. Please use either 'facets' or 'racetrack'")
  }
}

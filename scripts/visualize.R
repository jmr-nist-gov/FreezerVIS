checkPackages <- function() {
  if (require(ggthemes) == FALSE) {
    install.packages("ggthemes")
    library(ggthemes)
  }
  if (require(plotly) == FALSE) {
    install.packages("plotly")
    library(plotly)
  }
}


prepDat <- function(dat, freezer) {
  out <- dat %>% 
    filter(FreezerPhysName == freezer)
  out$CONTAINER_TYPE[is.na(out$CONTAINER_TYPE)] <- "Not Recorded"
  out$CONTAINER_TYPE <- factor(out$CONTAINER_TYPE)
  out <- droplevels(out)
  return(out)
}

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
                          levels = c(boxsort, tubesort))
  out$POSITION1 <- as.factor(out$POSITION1)
  return(out)
}

doBoxCount <- function(dat, freezer) {
  dat <- prepDat(dat, freezer)
  dat <- prepBoxCount(dat)
  dat <- dat %>%
    group_by(POSITION1, POSITION2, CONTAINER_TYPE) %>%
    summarise(nSAMPLES = n_distinct(POSITION3))
  return(dat)
}

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


vizFreezer <- function(dat, freezer, usePlotly=TRUE, saveMe=FALSE, saveFile=FALSE) {
  checkPackages()
  if (freezer %in% dat$FreezerPhysName) {
    out <- vizBoxCount(dat, freezer)
    if (usePlotly) {
      if (is.null(out[[1]]) & is.null(out[[2]])){
        out <- paste("No data found for", freezer)
      } else if (is.null(out[[1]])) {
        out <- ggplotly(out[[2]]) %>%
          layout(margin = list(b = 40, l = 50, t = 30),
                 legend = list(y = 0.97))
      } else if (is.null(out[[2]])) {
        out <- ggplotly(out[[1]]) %>%
          layout(margin = list(b = 40, l = 50, t = 30),
                 legend = list(y = 0.97))
      } else {
        out <- subplot(ggplotly(out[[1]]),
                style(ggplotly(out[[2]]), showlegend = FALSE),
                nrows = 2,
                heights = c(0.7, 0.25),
                titleX = TRUE,
                titleY = TRUE) %>%
          layout(margin = list(b = 40, l = 50, t = 30),
                 legend = list(y=0.97))
      }
      print(out)
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
    cat(paste0("Freezer '", freezer, "' not found."))
  }
}

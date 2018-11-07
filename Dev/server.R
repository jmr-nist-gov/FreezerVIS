# Define server logic required for FreezerVIS

server <- function(input, output, session) {
  
  # Refresh Data button clicked, ignoreNULL triggers run on load.
  observeEvent(input$refresh, ignoreNULL = FALSE, {
    shinyjs::addClass("mask", "overlay")
    shinyjs::removeClass("mask", "hidden")
    if (!validConnection(dsn)) {
      counts <<- read_rds("data/counts.RDS")
      shinyjs::addClass("status", "warning")
      output$status <- renderText(paste0("Could not connect to Freezerworks! Using archived data (", as.Date(attr(counts, "asof")), ")."))
    } else {
      shinyjs::removeClass("status", "warning")
      refreshData(dsn)
      counts <<- read_rds("data/counts.RDS")
      output$status <- renderText(paste("Data last refreshed", attr(counts, "asof")))
    }
    if (!is.null(counts)) {
      output$bankView1 <- renderPlot(vizSpace('facets', showAll = FALSE))
      output$bankView2 <- renderPlot(vizSpace('track', showAll = FALSE))
    }
    lapply(1:dim(basketCounts)[1], function (i) {
      output[[basketCounts$useOutputId[i]]] <- renderUI({
            sliderInput(inputId = basketCounts$useInputId[i],
                        label = basketCounts$freezerphysname[i],
                        width = "100%",
                        ticks = FALSE,
                        value = basketCounts$value[i],
                        min = 0,
                        max = basketCounts$maxvalue[i],
                        step = 1)
          })
    })
    updateSelectInput(session = session,
                      inputId = "selectFreezer",
                      choices = sort(unique(counts$FreezerPhysName)),
                      selected = NULL)
    shinyjs::addClass("mask", "hidden")
    shinyjs::removeClass("mask", "overlay")
  })
  
  # TAB REPOSITORY VIEW------------------------------------------------------------------------------
  ## Change to all freezers.
  observeEvent(input$showSome, {
    hrz_cols <- 8
    output$bankView1 <- renderPlot(vizSpace('facets', showAll = !input$showSome, n_columns = hrz_cols))
    output$bankView2 <- renderPlot(vizSpace('track', showAll = !input$showSome))
    # output$badContainers <- renderPlot(vizSpace('bad', showAll = !input$showSome, usePlotly = FALSE))
    output$badContainers <- renderPlotly(vizSpace('bad', showAll = !input$showSome, usePlotly = TRUE))
  })
  
  ## Click on a facet in the Specific Freezers view
  observeEvent(input$clickFacet, {
    # Catch shiny packages prior to 1.1.0
    ### This is made particularly difficult as the plots are created on input$showSome click and not stored
    ### and since the default methods in prior versions of shiny do not support heavily faceted charts well.
    ### This manual hack around should work, but may become unstable if the number of freezers changes or
    ### we decide to display a different number per row by changing the n_columns argument to vizSpace().
    ### These 30 lines are eliminated down to 1 in version 1.1.0. This section commented out in favor of
    ### adding a recommendation to shiny 1.1.0 and ggplot2 3.0 since keeping the scaling while changing plot
    ### output sizes was troublesome to say the least.
    ###   - JMR 20160713
    frz_selected <- NULL
    # if (packageVersion('shiny')[1] < 1.1 | packageVersion('ggplot2')[1] < 3) {
    #   clicked_x <- input$clickFacet$x
    #   clicked_y <- input$clickFacet$y
    #   n_cols <- hrz_cols
    #   n_rows <- ifelse(hrz_cols == 10, 4, 5)
    #   if (input$showSome){
    #     x_offset <- 241
    #     x_box <- 160
    #     y_offset <- 75
    #     y_box <- 205
    #     frzs <- sort(spaceDat$freezer_plot_order_same)
    #   } else {
    #     x_offset <- 10
    #     x_box <- 158
    #     y_offset <- 78
    #     y_box <- 203
    #     frzs <- sort(spaceDat$freezer_plot_order_all)
    #   }
    #   max_valid_x <- x_box * n_cols + x_offset
    #   max_valid_y <- y_box * n_rows + y_offset
    #   frzs <- c(frzs, rep(NA, n_rows*n_cols - length(frzs)))
    #   frzs <- matrix(frzs, 
    #                  nrow = n_rows, 
    #                  ncol = n_cols, 
    #                  byrow = TRUE)
    #   if (between(clicked_x, x_offset, max_valid_x) & between(clicked_y, y_offset, max_valid_y)) {
    #     x_selected <- ceiling((clicked_x - x_offset) / x_box)
    #     y_selected <- ceiling((clicked_y - y_offset) / y_box)
    #     frz_selected <- frzs[y_selected, x_selected]
    #   }
    # } else {
      frz_selected <- strsplit(input$clickFacet$panelvar1, "\n")[[1]][1]
      frz_selected <- gsub("Upright", "Upright Freezer", frz_selected)
    # }
    if (!frz_selected %in% freezers$FreezerPhysName) {
      alert(paste(frz_selected, "is not being tracked by Freezerworks. Space estimation is from basket counts only."))
      return(NULL)
    }
    if (!is.null(frz_selected)) {
      updateSelectInput(session,
                        inputId = 'selectFreezer',
                        selected = frz_selected)
      updateTabsetPanel(session,
                        inputId = 'nav',
                        selected = 'Freezer View')
    }
  })
  #--------------------------------------------------------------------------------------------------
  
  # TAB FREEZER VIEW---------------------------------------------------------------------------------
  ## Freezer select input changed.
  observeEvent(input$selectFreezer, {
    # viz <- vizFreezer(counts, input$selectFreezer, inApp = TRUE, usePlotly = TRUE)
    viz <- suppressWarnings(vizFreezerPlotly(counts, input$selectFreezer))
    if (!is.null(viz)) {
      output$plotBoxes <- renderPlotly(viz)
    }
  })
  #--------------------------------------------------------------------------------------------------
  
  # TAB INFORMATION VALUES---------------------------------------------------------------------------
  ## Control to update the basket counts and update space estimates.
  observeEvent(input$buttonUpdateBasketCount, {
    x <- reactiveValuesToList(input)
    x <- x[which(grepl('slide', names(x)) == TRUE)]
    tmp <- data.frame('useInputId' = names(x),
                      'value' = unlist(t(x)))
    basketCounts <- basketCounts %>% 
      select(-value) %>% 
      left_join(tmp)
    saveRDS(basketCounts, "data/basketCounts.RDS")
    basketCounts <<- basketCounts
    spaceDat <<- spaceUsed()
    hrz_cols <- 8
    output$bankView1 <- renderPlot(vizSpace('facets', showAll = !input$showSome, n_columns = hrz_cols))
    output$bankView2 <- renderPlot(vizSpace('track', showAll = !input$showSome, n_columns = hrz_cols))
  })
  
  ## Generate the table for container type ratios
  output$tableCTRatios <- renderDT(DT::datatable(containerRatios %>%
                                                   arrange(desc(CT_Ratio), CONTAINER_TYPE),
                                                 filter = 'top', 
                                                 colnames = c("Container Type", "Storage space required relative to a 2 mL cryovial"),
                                                 rownames = FALSE) %>%
                                     formatRound('CT_Ratio', 2))
  
  ## Generate the table for freezer capacity
  output$tableCapacities <- renderDT(DT::datatable(freezers %>%
                                                     select(FreezerPhysName, Description) %>%
                                                     arrange(FreezerPhysName) %>%
                                                     left_join(freezerCapacity, by = c("Description" = "DESCRIPTION")) %>%
                                                     filter(!is.na(Size)) %>%
                                                     mutate(Size = recode(Size, 
                                                                          S = "Small", 
                                                                          M = "Medium", 
                                                                          L = "Large",
                                                                          V = "Vario"),
                                                            FreezerPhysName = as.factor(FreezerPhysName),
                                                            Description = as.factor(Description),
                                                            Capacity = comma(Capacity)),
                                                   filter = 'top', 
                                                   colnames = c("Freezer", "Description", "Size", "2 mL Cryovial Capacity"),
                                                   rownames = FALSE))
  #---------------------------------------------------------------------------------------------------
  
  session$onSessionEnded(function() {
	  stopApp()
  })
}
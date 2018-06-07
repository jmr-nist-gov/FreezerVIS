# Define server logic required for FreezerVIS

server <- function(input, output, session) {
  
  dsn <- read_rds("data/DSN.RDS")
  
  if (isolate(input$refresh) == 0) {
    if (!validConnection(dsn)) {
      shinyjs::addClass("status", "warning")
      counts <- read_rds("data/counts.RDS")
      output$status <- renderText(paste("Could not connect to", dsn, "! Using archived data (", as.Date(attr(counts, "asof")), ")."))
    } else {
      shinyjs::removeClass("status", "warning")
      shinyjs::disable("selectFreezer")
      shinyjs::addClass("mask", "overlay")
      shinyjs::removeClass("mask", "hidden")
      refreshData(session, output, dsn)
      counts <- read_rds("data/counts.RDS")
      output$status <- renderText(paste("Data last refreshed", attr(counts, "asof")))
      shinyjs::addClass("mask", "hidden")
      shinyjs::removeClass("mask", "overlay")
      shinyjs::enable("selectFreezer")
    }
  }
  updateSelectInput(session = session,
                    inputId = "selectFreezer",
                    choices = sort(unique(counts$FreezerPhysName)),
                    selected = NULL)
  observeEvent(input$refresh, {
    if (!validConnection(dsn)) {
      shinyjs::addClass("status", "warning")
      counts <- read_rds("data/counts.RDS")
      output$status <- renderText(paste("Could not connect to", dsn, "! Using archived data (", as.Date(attr(counts, "asof")), ")."))
    } else {
      shinyjs::removeClass("status", "warning")
      shinyjs::disable("selectFreezer")
      shinyjs::addClass("mask", "overlay")
      shinyjs::removeClass("mask", "hidden")
      refreshData(session, output, dsn)
      counts <- read_rds("data/counts.RDS")
      output$status <- renderText(paste("Data last refreshed", attr(counts, "asof")))
      shinyjs::addClass("mask", "hidden")
      shinyjs::removeClass("mask", "overlay")
      shinyjs::enable("selectFreezer")
    }
    updateSelectInput(session = session,
                      inputID = "selectFreezer",
                      choices = sort(unique(counts$FreezerPhysName)),
                      selected = NULL)
  })
  
  observeEvent(input$selectFreezer, {
    viz <- vizFreezer(counts, input$selectFreezer, inApp = TRUE, usePlotly = TRUE)
    if (!is.null(viz)) {
      output$plotBoxes <- renderPlotly(viz)
    }
  })
  
  session$onSessionEnded(function() {
	stopApp()
  })
}
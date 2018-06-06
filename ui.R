# Documentation

ui <- fluidPage(
  title = "NIST Freezer VIS",
  div(id="mask", class="hidden", img(src="processing.gif"), h3("Refreshing collection data...")),
  useShinyjs(),
  tags$head(tags$style(HTML("
    .rightAlign {float:right; display:block;}
    #status {font-size:10px; text-align:right;}
    .warning {color:red; font-weight:bold}
    .hidden {position:absolute; z-index:1;}
    .overlay {position: absolute; z-index: 3; opacity: 0.85; top: 0; bottom: 0; left: 0; right: 0; width: 100%; height: 100%; background-color: White; color: Black;}
    .overlay>img {position: absolute; top: 50%; left: 50%; width: 200px; height: 200px; margin-top: -100px; margin-left: -100px; opacity: 1;}
    .overlay>h3 {position: absolute; top: 50%; left: 50%; width: 200px; height: 200px;; margin-top: 100px; margin-left: -100px; color: Black; opacity: 1; text-align: center}
    "))
  ),
   
   # Effective Header Page
   fluidRow(
     column(8,
            h2("NIST Marine Environmental Specimen Bank"),
            h3("Freezer Visual Information System")),
     column(4,
            br(),
            img(src="A product of NIST DTD.png", height = 60, width = 233, class = 'rightAlign'),
            br(),
            br(),
            br(),
            actionButton(inputId = 'refresh',
                         label = 'Refresh Data',
                         icon = icon('refresh', lib = 'glyphicon'),
                         width = '50%',
                         class = 'rightAlign'),
            br(),
            br(),
            textOutput(outputId = 'status')
     )
   ),
   hr(),
   fluidRow(
     column(12,
            selectInput(inputId = 'selectFreezer',
                        label = 'Select a Freezer',
                        choices = NULL,
                        selected = NULL,
                        multiple = FALSE,
                        width = '100%')
     )
   ),
   fluidRow(
     column(12,
            plotlyOutput(outputId = 'plotBoxes', 
                         inline = T, 
                         width = "100%", 
                         height = "1000px")
     )
   )
)
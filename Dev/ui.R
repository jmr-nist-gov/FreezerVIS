# Documentation


ui <- fluidPage(
  title = "NIST Freezer VIS",
  div(id="mask", class="hidden", img(src="processing.gif"), h3("Refreshing collection data...")),
  useShinyjs(),
  tags$head(tags$style(HTML("
    .rightAlign {float: right; display: block;}
    #status {font-size: 10px; text-align: right;}
    .warning {color: red; font-weight:bold}
    .hidden {position: absolute; z-index: 1;}
    .overlay {position: absolute; z-index: 3; opacity: 0.85; top: 0; bottom: 0; left: 0; right: 0; width: 100%; height: 100%; background-color: White; color: Black;}
    .overlay>img {position: absolute; top: 50%; left: 50%; width: 200px; height: 200px; margin-top: -100px; margin-left: -100px; opacity: 1;}
    .overlay>h3 {position: absolute; top: 50%; left: 50%; width: 200px; height: 200px;; margin-top: 100px; margin-left: -100px; color: Black; opacity: 1; text-align: center;}
    .basket_slider {display: inline-block; padding: 15px; margin: 10px; border-style: groove; width: 200px}
    #bankView1 {max-width: 100%; float: left; text-align: center;}
    #plotBoxes {position: relative; padding: 0px; top: 0px; left: 0px;}
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
  tabsetPanel(id = 'nav',
              tabPanel(title = 'Repository View',
                       checkboxInput(inputId = 'showSome',
                                     label = 'Show only freezers with good estimation.', 
                                     value = TRUE),
                       tabsetPanel(id = 'birdView', type = 'pills',
                                   tabPanel('Specific Freezers',
                                            br(),
                                            fluidRow(
                                              # column(12,
                                                     plotOutput(outputId = 'bankView1',
                                                                inline = FALSE,
                                                                width = '100%',
                                                                height = '1000px',
                                                                click = 'clickFacet')
                                              # )
                                            )
                                   ),
                                   tabPanel('Relative to Bank',
                                            br(),
                                            fluidRow(
                                              # column(12,
                                                     plotOutput(outputId = 'bankView2',
                                                                inline = FALSE,
                                                                width = '100%',
                                                                height = '1400px')
                                              # )
                                            )
                                   ),
                                   tabPanel('Bad Containers',
                                            br(),
                                            fluidRow(
                                              column(12,
                                                     # plotOutput(outputId = 'badContainers',
                                                     #            inline = FALSE,
                                                     #            width = '100%',
                                                     #            height = '1000px')
                                                     plotlyOutput(outputId = 'badContainers',
                                                                  inline = FALSE,
                                                                  width = '100%',
                                                                  height = '1000px')
                                              )
                                            )
                                   )
                       )
              ),
              tabPanel(title = 'Freezer View',
                       br(),
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
                                             inline = TRUE,
                                             width = '100%',
                                             height = '1000px')
                         )
                       )
              ),
              tabPanel(title = 'Information Values',
                       tabsetPanel(id = 'setValues', type = 'pills',
                                   tabPanel('Baskets per Freezer',
                                            br(),
                                            fluidRow(
                                              actionButton('buttonUpdateBasketCount',
                                                           'Save Basket Counts',
                                                           icon = icon('save'),
                                                           width = "100%")
                                            ),
                                            fluidRow(
                                              lapply(1:dim(basketCounts)[1], function(i) {
                                                uiOutput(outputId = basketCounts$useOutputId[i],
                                                         class = 'basket_slider')
                                              })
                                            )
                                   ),
                                   tabPanel('Container Type Ratios',
                                            br(),
                                            DTOutput('tableCTRatios')
                                            ),
                                   tabPanel('Freezer Capacities',
                                            br(),
                                            DTOutput('tableCapacities')
                                   )
                       )
              )

  )
)
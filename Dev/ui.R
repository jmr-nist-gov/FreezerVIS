# Documentation


ui <- fluidPage(
  title = "NIST Freezer VIS",
  div(id="mask", class="hidden", img(src="processing.gif"), h3("Refreshing collection data...")),
  useShinyjs(),
  tags$head(tags$style(HTML("
    .rightAlign {float: right; display: inline-block;}
    .leftAlign {float: left; display: inline-block;}
    hr {margin-top: 10px; margin-bottom: 10px;}
    #status {font-size: 10px; text-align: right; display: block}
    #refresh {float: right; display: block; margin-top: 20px; width: 100%}
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
    column(9,
           # h2("NIST Marine Environmental Specimen Bank"),
           h2("Freezer Visual Information System (development version)")
           ),
    column(3,
           # img(src="DTD_logo_left.png", width = 259, class = 'rightAlign'),
           actionButton(inputId = 'refresh',
                        label = 'Refresh Data',
                        icon = icon('refresh', lib = 'glyphicon')),
           textOutput(outputId = 'status')
           )#,
    # column(2,
    #        br(),
    # )
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
                                                                height = '1400px',
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
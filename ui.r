library(shiny)

shinyUI(pageWithSidebar(
             headerPanel("OpCost"),
             
             sidebarPanel(img(src = "UI.png",height = 100, width = 100),
                          
                          img(src = "Forest_Operations.png", height = 100, width = 100),
               
               conditionalPanel(condition="input.conditionedPanels=='Tables'",
                                
                                h4("Choose FRCS Batch File Text File"),
                                
                                fileInput('file1', '',
                         
                                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.txt')),
                                
                                h5("Choose Assumed Labor Costs"),
                                
                                selectInput('state', 'Select State of Operation', choices=c('Idaho',
                                                                  'Oregon',
                                                                  'Washington')),
                                
                                h5("Choose Which Cost Information to Calculate"),                              
                                                        
                                checkboxInput('idealChoice',
                                              label="Simulated Cheapest Future Cost",
                                              value=FALSE),
                                
                                h5("Additional Information for Present Cost"),
                                
                                numericInput("start", "Input Fiscal Cycle Starting Year", value=2016),
                                
                                numericInput("interest", "Input Assumed Decimal Interest Rate", value=.05),
                                
                                checkboxInput('presentChoice',
                                              label="Present Cost of Prescribed Treatments",
                                              value=FALSE),
                                
                                checkboxInput('presentIdealChoice',
                                              label="Present Cost of the Simulated Cheapest Operations",
                                              value=FALSE),
                              
                                
                                h5("Download Cost Estimations as a Text File"),
                                
                                selectInput('downloadSelect', 'Select Datatable To Download', 
                                            choices=c('Prescribed Future Cost Table',
                                                      'Simulated Cheapest Future Cost Table',
                                                      'Prescribed Present Cost Table',
                                                      'Simulated Cheapest Present Cost Table')),
                                
                                downloadButton('downloadData', 'Download')),
               
               conditionalPanel(condition="input.conditionedPanels=='Plots'",
                                
                                h4("Choose Plots to Display"),
                                
                                h6("*Need to Calculate Costs Before the Coresponding Plots can be Created"),
                                
                                h6("**All plots can be copied by right clicking on them"),
                                                               
                                checkboxInput('plotChoice',
                                              label="Future Cost of Prescribed Operations",
                                              value=FALSE),
                                
                                checkboxInput('idealPlotChoice',
                                              label="Future Cost of the Simulated Cheapest Operations",
                                              value=FALSE),
                                
                                checkboxInput('presentPlotChoice',
                                              label="Present Cost of Prescribed Operations",
                                              value=FALSE),
                                
                                checkboxInput('presentIdealPlotChoice',
                                              label="Present Cost of the Simulated Cheapest Operations",
                                              value=FALSE),
                                
                                checkboxInput('futureComboChoice',
                                              label="Combined Future Cost Plot",
                                              value=FALSE)
                                
                                ),
               
               conditionalPanel(condition="input.conditionedPanels=='Custom Cost/Hour'",
                                
                                h4("Turn on Custom Costs/Hour and Enter Costs"),
                                
                                checkboxInput("customChoice",
                                              label="Turn on Custom Costs",
                                              value=FALSE),
                                
                                numericInput("fellerBuncher", "Input Feller Buncher", value=143),
                                
                                numericInput("forwarder", "Input Forwarder", value=150),
                                
                                numericInput("harvester", "Input Harvester", value=143),
                                
                                numericInput("grappleSkidder", "Input Grapple Skidder", value=120),
                                
                                numericInput("cableSkidder", "Input Cable Skidder", value=111),
                                
                                numericInput("wheelFeller", "Input Wheeled Feller", value=143),
                                
                                numericInput("crawlerFeller", "Input Crawler Feller", value=169),
                                
                                numericInput("yarder", "Input Yarder", value=225),
                                
                                numericInput("slideboom", "Input Slideboom", value=200),
                                
                                numericInput("chainsaw", "Input Manual Feller", value=25),
                                
                                numericInput("chipper", "Input Chipper", value=100)
                                
                                )
               
               ),
             
             mainPanel(
               tabsetPanel(id = "conditionedPanels",
                           
                           tabPanel('Tables',
                                    
                                    textOutput('title1'),
                                    
                                    tableOutput('contents'),
                                    
                                    textOutput('title3'),
                                    
                                    tableOutput('ideal'),
                                    
                                    textOutput('title4'),
                                    
                                    tableOutput('presentTable'),
                                    
                                    textOutput('title5'),
                                    
                                    tableOutput('presentIdealTable')),
                                          
                           tabPanel('Plots',
                                     textOutput('title2'),
                                    
                                     plotOutput('costPlot'),
                                    
                                    textOutput('title7'),
                                    
                                    plotOutput('idealPlot'),
                                    
                                    textOutput('title6'),
                                    
                                    plotOutput('presentPlot'),
                                    
                                    textOutput('title8'),
                                    
                                    plotOutput('presentIdealPlot'),
                                    
                                    textOutput('title9'),
                                    
                                    plotOutput('futureComboPlot')),
                           
                           tabPanel('Custom Cost/Hour',
                                    
                                    tableOutput("values")
                                    ))
               
               

  )
))


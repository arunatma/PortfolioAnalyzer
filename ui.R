library(shiny)

shinyUI(navbarPage("MF Portfolio Scanner",
    tabPanel("Portfolio",
        sidebarLayout(
            sidebarPanel(
                p("You need to upload your portfolio excel file"),
                downloadButton('downloadData', 'Download Sample Portfolio Excel'),
                br(),
                br(),
                br(),
                tags$hr(),
                fileInput("file1", "Choose Portfolio Excel File",
                    accept = c(".xlsx")
                ),
                tags$hr(),
                checkboxInput("header", "Header", TRUE)

            ),
            mainPanel(
                fluidRow(
                    h1("Portfolio"),
                    dataTableOutput("contents")
                )
            )
        )
    ),

    tabPanel("Consolidated Stocks",
        sidebarLayout(
            sidebarPanel(
            ),
            mainPanel(
                fluidRow(
                    dataTableOutput("consolidated")
                )
            )
        )
    ),
    tabPanel("Sector Split",
        sidebarLayout(
            sidebarPanel(
            ),
            mainPanel(
                fluidRow(
                    dataTableOutput("sectorConsolidated")
                )
            )
        )
    ),
    tabPanel("Instrument Split",
        sidebarLayout(
            sidebarPanel(
            ),
            mainPanel(
                fluidRow(
                    dataTableOutput("instrumentConsolidated")
                )
            )
        )
    ),
    tabPanel("Stocks in MF",
        sidebarLayout(
            sidebarPanel(
            ),
            mainPanel(
                fluidRow(
                    dataTableOutput("rootStocks")
                )
            )
        )
    )    
))
    

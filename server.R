library(shiny)
source("portfolioUtil.R")

fromInternet <<- FALSE

shinyServer(function(input, output, session) {
  
    output$contents <- renderDataTable({
        inFile <- input$file1

        if (is.null(inFile))
            return(NULL)

        ownerPortfolio <- data.table(read.xlsx(inFile$datapath, sheetName="Portfolio", 
            stringsAsFactors=FALSE, header = input$header))
        
        if(fromInternet){
            portfolioSplits <- lapply(ownerPortfolio$URL, getPortfolio)
            for(i in c(1:length(portfolioSplits))){
                write.csv(portfolioSplits[[i]], ownerPortfolio$Filename[i])
            }
        } else {
            portfolioSplits <- list()
            csvFiles <- list.files(path = ".", pattern = "*.csv")
            for(i in c(1:length(csvFiles))){
                portfolioSplits[[i]] <- data.table(read.csv(csvFiles[i]))
            }
        }
        
        for(i in c(1:length(portfolioSplits))){
            portfolioSplits[[i]][, Inv := ownerPortfolio$Value[i] * as.double(as.character(Weight)) / 100]
            portfolioSplits[[i]][, MF := ownerPortfolio$Filename[i]]
        }

        allFunds <<- do.call("rbind", portfolioSplits)

        consolidated <<- allFunds[, list(Value = sum(Inv)), by=list(Stock, Instrument, Sector)]
        consolidated[, Wt := round(Value * 100 / sum(Value), 2)]
        consolidated <- consolidated[order(-Wt)]

        sectorConsolidated <<- consolidated[, list(Value=sum(Value)), by=list(Sector)]
        sectorConsolidated[, Wt := round(Value * 100 / sum(Value), 2)]
        sectorConsolidated <- sectorConsolidated[order(-Wt)]

        instrumentConsolidated <<- consolidated[, list(Value=sum(Value)), by=list(Instrument)]
        instrumentConsolidated[, Wt := round(Value * 100 / sum(Value), 2)]
        instrumentConsolidated <- instrumentConsolidated[order(-Wt)]

        rootStocks <<- allFunds[, list(Value = sum(Inv), WtInMf=Weight, Instrument, Sector), by=list(Stock, MF)][order(Stock)]
        rootStocks[, SplitPct := round(Value * 100 / sum(Value), 2), by=list(Stock)]
        rootStocks[, ValueOfMf := Value * 100 / as.double(as.character(WtInMf))]
        rootStocks[, StockTotalVal := sum(Value), by=list(Stock)]
        
        contentsLoaded <<- TRUE
        return(ownerPortfolio)
    })
    
    output$consolidated <- renderDataTable(
        consolidated
    )
    output$sectorConsolidated <- renderDataTable(
        sectorConsolidated
    )
    output$instrumentConsolidated <- renderDataTable(
        instrumentConsolidated
    )
    output$rootStocks <- renderDataTable(
        rootStocks
    )
    
})

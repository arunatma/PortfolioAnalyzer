library(XML)
library(data.table)
library(xlsx)

getPortfolio <- function(mfURL){
# Get the Detailed Portfolio of a MF scheme from Morningstar URL and reformat 
# it in a consumable format.  Returns a data table with portfolio composition
    xx <- readHTMLTable(mfURL)
    yy <- data.table(xx[5][[1]])
    # Name the data table 
    names(yy) = c("dummy1", "Stock", "Instrument", "Sector", 
        "dummy2", "Weight", "MarketValue", "NumberOfShares", 
        "PrevNumber", "FirstBought", "dummy3")
        
    # remove unwanted columns
    yy[, dummy1 := NULL]
    yy[, dummy2 := NULL]
    yy[, dummy3 := NULL]

    totalRows <- which(grepl(".*Total.*", yy$Stock))
    newRow <- which(grepl(".*New addition.*", yy$Stock))
    trailRows <- c()
    if(length(newRow) == 1) {
        trailRows <- c(newRow:dim(yy)[1])
    }
    removableRows <- unique(c(totalRows, trailRows))

    yy <- yy[!removableRows]

    # remove rows where market value is null 
    yy <- yy[which(MarketValue != "")]
    yy <- yy[which(yy$Weight != "-")]
    finalSet <- yy
    finalSet[, MFURL := mfURL]
    print(sum(as.double(as.character(finalSet$Weight))))       # 100 
    return(finalSet)
}

getStockSplit <- function(allFunds, inStock){
# allFunds is a data table that contains details on stock investment based
# on each MF scheme.  So, there will be multiple rows in the table for the same
# stock pertaining to the stock being the constituent in multiple MF schemes
# getStockSplit: Given a stock, gives out a table containing the details of 
# the MF schemes that the stock is present in.
    specificStock <- allFunds[Stock == inStock]
    specificStock[, SplitPct := round(Inv * 100 / sum(Inv), 2)]
    specificStock[, ValueOfMf := Inv * 100 / as.double(as.character(Weight))]
    specificStock[, StockValueInMF := Inv]
    outTable <- specificStock[, .(Stock, Instrument, 
        Sector, MF, ValueOfMf, StockValueInMF, SplitPct)][order(-SplitPct)]
    return (list(outTable, sum(outTable$StockValueInMF)))
}

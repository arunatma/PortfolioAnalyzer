library(XML)
library(data.table)
library(xlsx)
source("portfolioUtil.R")

# The individual portfolio comprising various MF Schemes.
# 3 Columns: 
# Column 1: Name of the csv file where the MF scheme portfolio to be stored
# Column 2: Market value in the said MF scheme 
# Column 3: Morningstar URL where the MF scheme details to be fetched from
ownerPortfolio <- data.table(read.xlsx("SamplePortfolio.xlsx", 
    sheetName="Portfolio", stringsAsFactors=FALSE))

fromInternet <- TRUE
if(fromInternet){
    # Now, call the getPortfolio function for each of the given URL in file     
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

allFunds <- do.call("rbind", portfolioSplits)

# Overall Consolidation
consolidated <- allFunds[, list(Value = sum(Inv)), by=list(Stock, Instrument, Sector)]
consolidated[, Wt := round(Value * 100 / sum(Value), 2)]
consolidated <- consolidated[order(-Wt)]

# Sector wise consolidation
sectorConsolidated <- consolidated[, list(Value=sum(Value)), by=list(Sector)]
sectorConsolidated[, Wt := round(Value * 100 / sum(Value), 2)]
sectorConsolidated <- sectorConsolidated[order(-Wt)]

# Instrument wise consolidation
instrumentConsolidated <- consolidated[, list(Value=sum(Value)), by=list(Instrument)]
instrumentConsolidated[, Wt := round(Value * 100 / sum(Value), 2)]
instrumentConsolidated <- instrumentConsolidated[order(-Wt)]

# Details on all stocks with respect to how much value is present in which MF 
# scheme, what is the percentage of total stock value being present in various 
# MF schemes and the total value of the stock in the owner portfolio
rootStocks <- allFunds[, list(Value = sum(Inv), WtInMf=Weight, Instrument, Sector), by=list(Stock, MF)][order(Stock)]
rootStocks[, SplitPct := round(Value * 100 / sum(Value), 2), by=list(Stock)]
rootStocks[, ValueOfMf := Value * 100 / as.double(as.character(WtInMf))]
rootStocks[, StockTotalVal := sum(Value), by=list(Stock)]

# Same as rootStocks, but for a given individual stock.
stockDetails <- getStockSplit(allFunds, "HDFC Bank Ltd")
stockSplit <- stockDetails[[1]]
stockValue <- stockDetails[[2]]

# These two values should match
sum(ownerPortfolio$Value)
sum(consolidated$Value)

# Some graphing!
consolidated[, CumWt := cumsum(Wt)]
plot(consolidated$CumWt, type="l")

# To display top Wt stocks
head(consolidated[order(-Wt)], 15)
write.csv(consolidated[order(-Wt)], "consolidated.csv")

# To search for a stock. 
consolidated[which(grepl(".*ver.*", Stock))]

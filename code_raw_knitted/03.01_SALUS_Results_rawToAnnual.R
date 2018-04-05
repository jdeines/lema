# Process SALUS Raw Results
# Jill Deines
# 5 December 2017

# Goal: Extract desired data subsets from the raw SALUS output for file size
# reduction and format for subsequent analyses

# Packages Required
library(tidyverse)

# arguments
# raw salus output directory
scratchDir <- 'F:/Users/deinesji/salusRawResults/7_lema_historic_realEst_v02'

# directory for cleaned/summarized output
outDir <- 'S:/Users/deinesji/HPA/Salus/lema/results/7_lema_hist_realEst_v02'

# run name
runname <- '7_lema_historic_realEst_v02'

startYear <- 2006 # excluding wheat planting, aka first year of harvest
endYear <- 2016


# define function 
salusRawToAnnual <- function(scratchDir, outDir, runname, startYear){
  # load daily results within scratchDir
  dailyFiles <- list.files(scratchDir, pattern = '*daily.csv')
  
  # load daily files, get annual non-cumulative totals, and combine into 1 df
  annualResults <- do.call(rbind, lapply(dailyFiles, function(x){
    # load file
    ddf <- read_csv(paste0(scratchDir, '/', x)) 
    
    # make an annual crop table - speciesID where GWAD is max
    cropByYear <- ddf %>%
      group_by(ExpID,Year) %>%
      slice(which.max(GWAD)) %>%
      select(c(ExpID,Year, SpeciesID))
    
    # get maximum values per year
    adf <- ddf %>%
      group_by(ExpID, Year) %>%
      summarize(GWAD = max(GWAD, na.rm=TRUE),
                IRRC_cum = max(IRRC, na.rm=TRUE),
                DRNC_cum = max(DRNC, na.rm=TRUE),
                PREC_cum = max(PREC, na.rm = TRUE),
                LAI = max(LAI, na.rm = TRUE)) %>%
      # add species name back in 
      left_join(cropByYear, by = c('ExpID','Year')) %>%
      # de-cumulate irrc, drnc, prec (turn to annual values)
      mutate(IRRC_mm = IRRC_cum - lag(IRRC_cum, default=0),
             DRNC_mm = DRNC_cum - lag(DRNC_cum, default=0),
             PREC_mm = PREC_cum - lag(PREC_cum, default=0)) %>%
        select(c(ExpID, Year, SpeciesID, GWAD, IRRC_mm, DRNC_mm, PREC_mm, LAI)) %>%
      filter(Year >= startYear)
    
    # add an irrigation flag
    adf$irrigation <- 'N'
    adf[adf$IRRC_mm > 0 ,'irrigation'] <- 'Y'
    
    return(adf)
  }))
  
  # write out
  write_csv(annualResults, path = paste0(outDir, '/', runname, '.csv'))
  return(annualResults)
}

# test
runResults <- salusRawToAnnual(scratchDir, outDir, runname, startYear)

# html_getTables test script

library(rlang)
library(MazamaCoreUtils)
library(MazamaSpatialPlots)

setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates_02")

# ----- Population -------------------------------------------------------------

popTbl <- html_getTable("http://www.theus50.com/fastfacts/population.php", 1)

# > dplyr::glimpse(popTbl)
# Observations: 50
# Variables: 3
# $ Rank              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
# $ `State Name`      <chr> "California", "Texas", "New York", "Florida", "Illinois",…
# $ `Population 2010` <chr> "37,253,956", "25,145,561", "19,378,104", "18,801,311", "…

# Clean up the table
popTbl <-
  popTbl %>%
  dplyr::mutate(
    rank = .data$Rank,
    stateCode = MazamaSpatialUtils::US_stateNameToCode(.data$`State Name`),
    stateName = .data$`State Name`,
    population = as.numeric( stringr::str_replace_all(.data$`Population 2010`, ',', '') )
  ) %>%
  dplyr::select(rank, stateCode, stateName, population)

# Plot the table
MazamaSpatialPlots::stateMap(
  popTbl,
  parameter = "population",
  breaks = c(0,1,2,5,10,20,50) * 1e6,
  title = "2010 Population"
)

# ----- Income Tax -------------------------------------------------------------

taxTbls <- html_getTables("https://www.tax-rates.org/taxtables/income-tax-by-state")

# > str(lapply(taxTbl, dim))
# List of 4
# $ : int [1:2] 1 3
# $ : int [1:2] 2 2
# $ : int [1:2] 13 5
# $ : int [1:2] 51 3

# Fourth one appears to have the states

taxTbl <-
  taxTbls[[4]] %>%
  dplyr::mutate(
    stateCode = MazamaSpatialUtils::US_stateNameToCode(.data$`State Name`),
    stateName = .data$`State Name`,
    lowestBracket = as.numeric( stringr::str_replace_all(.data$`Lowest Tax Bracket`, '%', '') ),
    highestBracket = as.numeric( stringr::str_replace_all(.data$`Highest Tax Bracket`, '%', '') )
  ) %>%
  dplyr::select(stateCode, stateName, lowestBracket, highestBracket)

# Plot the table
MazamaSpatialPlots::stateMap(
  taxTbl,
  parameter = "lowestBracket",
  title = "Lowest Tax Bracket"
)

# Plot the table
MazamaSpatialPlots::stateMap(
  taxTbl,
  parameter = "highestBracket",
  title = "Highest Tax Bracket"
)

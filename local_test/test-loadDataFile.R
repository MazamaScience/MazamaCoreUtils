library(MazamaCoreUtils)

filename = "USCensusStates_02.rda"
dir = "~/Data/Spatial"
url = "http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8"

# ----- Success tests ----------------------------------------------------------

# Load local file
USCensusStates = loadDataFile(filename, dataDir = dir)

# Load remote file
USCensusStates = loadDataFile(filename, dataUrl = url)

# Load local file with remote file as backup
USCensusStates =
  loadDataFile(filename, dataDir = dir, dataUrl = url, priority = "dataDir")

# Load remote file with local file as backup
USCensusStates =
  loadDataFile(filename, dataDir = dir, dataUrl = url, priority = "dataUrl")

# ----- Priority tests ---------------------------------------------------------

filename = "USCensusStates_02.rda"
dir = "~/NOT_FOUND"
url = "http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8"

# Load local file with remote file as backup
USCensusStates =
  loadDataFile(filename, dataDir = dir, dataUrl = url, priority = "dataDir")

dir = "~/Data/Spatial"
url = "http://data.mazamascience.com/NOT_FOUND/Spatial_0.8"

# Load remote file with local file as backup
USCensusStates =
  loadDataFile(filename, dataDir = dir, dataUrl = url, priority = "dataUrl")

# ----- Failure tests ----------------------------------------------------------

# * bad filename -----

filename = "NOT_FOUND.rda"
dir = "~/Data/Spatial"
url = "http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8"

# Load local file
USCensusStates = loadDataFile(filename, dataDir = dir)

# Load remote file
USCensusStates = loadDataFile(filename, dataUrl = url)

# Load local file with remote file as backup
USCensusStates =
  loadDataFile(filename, dataDir = dir, dataUrl = url, priority = "dataDir")

# Load remote file with local file as backup
USCensusStates =
  loadDataFile(filename, dataDir = dir, dataUrl = url, priority = "dataUrl")




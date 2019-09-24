# Testing examples on the vignettes page

# Cache Management
CACHE_DIR <- file.path(tempdir(), 'cache')
if ( file.exists(CACHE_DIR) == FALSE ) {
  dir.create(CACHE_DIR)
}
write.csv(matrix(1,400,500), file=file.path(CACHE_DIR,'m1.csv'))
Sys.sleep(1)
write.csv(matrix(2,400,500), file=file.path(CACHE_DIR,'m2.csv'))
Sys.sleep(1)
write.csv(matrix(3,400,500), file=file.path(CACHE_DIR,'m3.csv'))
Sys.sleep(1)
write.csv(matrix(4,400,500), file=file.path(CACHE_DIR,'m4.csv'))
cachedFiles <- list.files(CACHE_DIR, full.names = TRUE)
infoDF <- file.info(cachedFiles)
cacheSize = (sum(infoDF$size) / 1e6) # in MB
print(list.files(CACHE_DIR))
sprintf("Cache size = %s MB", cacheSize)
invisible( read.csv(file.path(CACHE_DIR, 'm1.csv')) )
invisible( read.csv(file.path(CACHE_DIR, 'm2.csv')) )
manageCache(CACHE_DIR, extensions = 'csv', maxCacheSize = 1)
cachedFiles <- list.files(CACHE_DIR, full.names = TRUE)
infoDF <- file.info(cachedFiles)
cacheSize = (sum(infoDF$size) / 1e6) # in MB
print(list.files(CACHE_DIR))
sprintf("Cache size = %s MB", cacheSize)

# Date Parsing














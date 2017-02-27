saveDir <- "applications-results/enron/data"

source("datFile.R")
load(file.path(saveDir, "edgeList.RData"))


# Making an ipairs file and saving
slDir <- file.path(saveDir, "../", "results")
if (!dir.exists(slDir))
  dir.create(slDir, recursive = TRUE)
write.table(edgeList, 
            file = file.path(slDir, "enron-network.ipairs"), 
            row.names = FALSE, 
            col.names = FALSE)


# Making the OSLOM2 folder and writing the dat file
osDir <- file.path(saveDir, "../", "results/OSLOM2")
if (!dir.exists(osDir))
  dir.create(osDir, recursive = TRUE)
write.table(edgeList, 
            file = file.path(osDir, "enron-network.dat"), 
            row.names = FALSE, 
            col.names = FALSE)
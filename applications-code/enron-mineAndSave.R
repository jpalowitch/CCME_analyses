# Function to mine a file
mineFile <- function(fn){
  
  cat(fn,"\n")
  fLines = suppressWarnings(readLines(fn))
  fLines <- unname(sapply(fLines, function (Line) iconv(enc2utf8(Line), sub = "byte")))
  toLineNum = suppressWarnings(try(min(which(sapply(fLines,function(Line)substr(Line,1,3)) == "To:"))))
  fromLineNum = suppressWarnings(try(min(which(sapply(fLines,function(Line)substr(Line,1,5)) == "From:"))))
  
  if(class(toLineNum) == "list" || class(fromLineNum) == "list" ||
       sum(abs(toLineNum) + abs(fromLineNum)) == Inf)
    return(list("Froms" = NULL,"Tos" = NULL))
  
  extraToLines = NULL
  nextLine = toLineNum + 1
  continueTo = substr(fLines[nextLine],1,1) == "\t"
  
  while(continueTo){
    extraToLines <- c(extraToLines, gsub("[\x01-\x1f\x7f-\xff]", "", 
                                         fLines[nextLine]))
    nextLine = nextLine + 1
    continueTo = substr(fLines[nextLine],1,1) == "\t"
  }
  
  if(!is.null(extraToLines))
    extraTos = as.vector(unlist(strsplit(extraToLines,", ")))
  
  toLine = gsub("[\x01-\x1f\x7f-\xff]", "", fLines[toLineNum])
  fromLine = gsub("[\x01-\x1f\x7f-\xff]", "", fLines[fromLineNum])
  Tos = strsplit(substr(toLine,5,nchar(toLine)),", ")
  Froms = strsplit(substr(fromLine,7,nchar(fromLine)),", ")
  
  if(!is.null(extraToLines)){
    Tos = as.vector(c(unlist(Tos),extraTos))
  }else{
    Tos = as.vector(c(unlist(Tos)))
  }
  Froms = as.vector(unlist(Froms))
  
  # Format closer to edges
  Tos_edges = rep(Tos,length(Froms))
  Froms_edges = as.vector(sapply(Froms,function(FromName)rep(FromName,length(Tos))))
  if(length(Tos_edges)*length(Froms_edges) == 0)
    return(list("Froms" = NULL,"Tos" = NULL))
  
  return(list("Froms" = Froms_edges, "Tos" = Tos_edges))
  
}


# A function to get from_tos from particular folder
mineFolder <- function(folder){
  fileList <- list.files(folder,recursive = TRUE,include.dirs = FALSE,full.names = TRUE)
  from_tos <- lapply(fileList,mineFile)
  names(from_tos) <- fileList
  return(from_tos)
}


# Get parent author foldernames
folderList = list.dirs("maildir",recursive = FALSE,full.names = TRUE)


# Mine all files and format closer to edges
from_tosList = rep(list(NULL),length(folderList))
for(i in 1:length(folderList))
  from_tosList[[i]] = mineFolder(folderList[i])


# Counting tos and froms
toCount = 0
fromCount = 0
for(i in 1:length(folderList)){
  from_tos = from_tosList[[i]]
  for(j in 1:length(from_tos)){
    toCount_j = length(from_tos[[j]]$Tos)
    fromCount_j = length(from_tos[[j]]$Froms)
    if(toCount_j != fromCount_j)
      break
    toCount = toCount + toCount_j
    fromCount = fromCount + fromCount_j
  }
  if(toCount_j != fromCount_j)
    break
  rm(from_tos)
}


# Check that the counts are equal
if (toCount != fromCount)
  stop('# in-edges != # out-edges')

# Save the things necessary to make edgeList in different session
if (!dir.exists("applications-results/enron/data"))
  dir.create("applications-results/enron/data", recursive = TRUE)
save(folderList,toCount,from_tosList,
     file = file.path("applications-results/enron/data", "mineAndSave_results.RData"))


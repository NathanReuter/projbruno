library(readxl);
SAVES_PATH = "./saves/";
#Method to convert the list into a ROW dataframe
listToROW <- function(data) {
  nCol <- max(vapply(data, length, 0))
  data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
  data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
  data.frame(data)
}

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

normalizedRowNumbers <- function(vectorList, size) {
  index =  which.max(lapply(vectorList, length));
  if (size == 0) {
    maxCol = length(vectorList[[index]]);
  } else {
    maxCol = size;
  }
  
  returnList = vector("list", length(vectorList));
  i = 0
  
  for (vec in vectorList) {
    i <- i + 1;
    returnList[[i]] <- c(vec, double(maxCol - length(vec)));
  }
  
  return (returnList);
}

parseDate <- function(DateString) {
  parsedYear = unlist(strsplit(toString(DateString), "-"))[1];
  
  return (parsedYear);
}

saveData <- function(file, name) {
  saveRDS(file, paste(SAVES_PATH, name, ".Rds",  sep = ""))
}

loadData <- function(name) {
  return(readRDS(paste(SAVES_PATH, name, ".Rds",  sep = "")));
}

#Fecht companies code
# Get all companies name, this will be use later
# allCompanies = gdfpd.get.info.companies(type.data = "companies")[[1]];
# # Test info to get ONE companie info
# name.companies <- allCompanies;
# name.companies <- setdiff(name.companies, c("MARAMBAIA ENERGIA RENOVÁVEL SA"));
# first.date <- '2010-01-01';
# last.date <- '2017-01-01';
# df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);
# save(df.statements,file="statements.Rda")



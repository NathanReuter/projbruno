# Set the file location as working directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
# Load local Scripts
source("utils.R");
source("sheetFunctions.R");
# Load GetDFP library
library("GetDFPData", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4");

# Load needed csv
brunoSheet = read_excel("resourceSheets/Bruno.xlsx");
processSheet = read_excel_allsheets("resourceSheets/processos.xlsx");

# Get all companies name, this will be use later
allCompanies = gdfpd.get.info.companies(type.data = "companies")[[1]];
# Test info to get ONE companie info
# This will be change to iterate through allCompanies List
name.companies <- 'ELETROPAULO METROPOLITANA EL.S.PAULO S.A';
first.date <- '2010-01-01';
last.date <- '2017-01-01';
df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);

setwd("~/projects/projetoBruno/progFiles")
#setwd("~/projbruno")
# Load local Scripts
source("utils.R");
source("sheetFunctions.R");
# Load GetDFP library
library("GetDFPData");
library("dplyr")

# Load needed csv
brunoSheet = read_excel("resourceSheets/Bruno.xlsx");
processSheet = read_excel_allsheets("resourceSheets/processos.xlsx");
processSheet = processSheet[7: length(processSheet)-1];
# Get all companies name, this will be use later
allCompanies = gdfpd.get.info.companies(type.data = "companies")[[1]];
# Test info to get ONE companie info

name.companies <- allCompanies[500:600];
first.date <- '2010-01-01';
last.date <- '2017-01-01';
df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);

#planilha1 <- p1(processSheet);

#planilha2 <- p2(df.statements);

# planilha3 = p3(df.statements)

planilha5 <- p5(df.statements);

#planilha7 <- p7(df.statements);

#planilha8 <- p8(df.statements);

#p9(df.statements);
#planilha10 <- p10(brunoSheet);

#planilha11 <- p11(df.statements);

#p12(brunoSheet);

p6 <- function(plan5) {
  sortedPlan = plan5[order(-plan5$Remuneração.Média),];
  index = 1;
  years = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017);
  RMRankitVector = vector();
  RRMVector = vector();
  
  for (year in years) {
    inYearComp = filter(sortedPlan, sortedPlan$Ano == year);
    total = nrow(inYearComp);
    for (index in 1:total) {
      RMRankit = total - index + 1;
      RMRankitVector <- c(RMRankitVector, RMRankit);
      RMM = round((RMRankit - 1)/(total -1), 2);
      RRMVector <- c(RRMVector, RMM);
    }
  }
  sortedPlan["RMRankit"] = RMRankitVector;
  sortedPlan["RMM"] = RRMVector;
  resultFrame = sortedPlan[order(-sortedPlan$Remuneração.Média),];
  View(resultFrame);
  
  return (resultFrame);
}

planilha6 <-p6(planilha5)
setwd("~/projects/projetoBruno/progFiles")
# Load local Scripts
source("utils.R");
source("sheetFunctions.R");
# Load GetDFP library
library("GetDFPData", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4");
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

# Load needed csv
brunoSheet = read_excel("resourceSheets/Bruno.xlsx");
processSheet = read_excel_allsheets("resourceSheets/processos.xlsx");
processSheet = processSheet[7: length(processSheet)-1];
# Get all companies name, this will be use later
allCompanies = gdfpd.get.info.companies(type.data = "companies")[[1]];
# Test info to get ONE companie info
# This will be change to iterate through allCompanies List
#name.companies <- 'ELETROPAULO METROPOLITANA EL.S.PAULO S.A';
name.companies <- allCompanies[1:10];
first.date <- '2010-01-01';
last.date <- '2017-01-01';
df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);

#planilha1 <- p1(processSheet);

#planilha10 <- p10(brunoSheet);

variableIncomePercentage <- function (dataInfo) {
  # get all variables. in history.compensation and divide to total.value.remunaration
  # And divid it in 3 columns Management Council, Statutory Directors, Fiscal Council
  # COLOCAR Data como apenas o Ano
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  MCVector = vector();
  SDVector = vector();
  FCVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hComp = company['history.compensation'];
    hComp = hComp$history.compensation[[1]];
    
    
    if (length(hComp) > 0) {
      localYearVector  = vector();
      localCompany = vector();
      localCode = vector();
      localMC = vector();
      localSD = vector();
      localFC = vector();
      cName = hComp[[1, 1]];
      calculateValue <- function(index) {
        varibleSum = hComp$variable.bonus[index] + hComp$variable.results.participation[index] 
        + hComp$variable.meetings.participation[index] + hComp$variable.others[index] + hComp$variable.results.participation[index];
        result = (varibleSum / hComp$total.value.remuneration[index]) * 100;
        if (is.nan(result)) {
          return (0)
        }
        
        return (round(result, 2));
      }
    
      for (index in seq_along(hComp$ref.date)) {
        parsedYear = unlist(strsplit(toString(hComp$ref.date[index]), "-"))[1];
        if (!(parsedYear %in% localYearVector)) {
          localYearVector <- c(localYearVector, parsedYear);
          localCompany <- c(localCompany, cName);
          localCode <- c(localCode, getCompanyCode(cName));
        }
        
        type = hComp$level.remuneration[index];
        value = calculateValue(index);
        switch (type,
                "Management Council" = localMC <- c(localMC, value),
                "Statutory Directors" = localSD <- c(localSD, value),
                "Fiscal Council" = localFC <- c(localFC, value)
        );
      }
      
      maxCol = max(length(localMC), length(localSD), length(localFC));
      localMC <- c(localMC, double(maxCol - length(localMC)));
      localSD <- c(localSD, double(maxCol - length(localSD)));
      localFC <- c(localFC, double(maxCol - length(localFC)));
      yearVector <<- c(yearVector, localYearVector);
      CompanyVector <<- c(CompanyVector, localCompany);
      CodeVector <<- c(CodeVector, localCode);
      MCVector <<- c(MCVector, localMC);
      SDVector <<- c(SDVector, localSD);
      FCVector <<- c(FCVector, localFC);
    }
  });
  View(CodeVector);
  View(CompanyVector);
  View(MCVector);
  View(SDVector);
  View(FCVector);
  
  resultFrame = data.frame(
    "Codigo" = CodeVector, 
    "Compania" = CompanyVector, 
    "Ano" = yearVector,
    "Management Council" = MCVector, 
    "Statutory Directors" = SDVector, 
    "Fiscal Council" = FCVector
  );
  View(resultFrame)
}

variableIncomePercentage(df.statements)
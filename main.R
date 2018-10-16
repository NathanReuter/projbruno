setwd("~/projbruno")
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
# This will be change to iterate through allCompanies List
name.companies <- allCompanies[1:300];
first.date <- '2010-01-01';
last.date <- '2017-01-01';
df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);

#planilha1 <- p1(processSheet);

# planilha2 <- p2(df.statements);

# planilha3 = p3(df.statements)

#planilha10 <- p10(brunoSheet);

#planilha5 <- p5(df.statements);

#planilha <- p7(df.statements);

#planilha8 <- p8(df.statements);
# p11 Dummy 1 - Novo Mercado ou Nível 2; 0 - Caso contrário
# history.gorvernance.listings
# listed.segment = "Novo Mercado" || "Nivel 2"
# Lista por ano e empresa
p11 <- function(dataInfo) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  DummyVector = vector();
  searchTarget = c("Novo Mercado", "Nivel 2");
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hGL = company$history.governance.listings[[1]];
    
    if (!is.null(hGL) && nrow(hGL) > 0) {
      localYearVector  = vector();
      localCompany = vector();
      localCode = vector();
      localDummyVector = vector();
      cName = hGL[[1, 1]];
      
      for (index in seq_along(hGL$ref.date)) {
        parsedYear = parseDate(hGL$ref.date[index])
        
        if (!(parsedYear %in% localYearVector)) {
          localYearVector <- c(localYearVector, parsedYear);
          localCompany <- c(localCompany, cName);
          localCode <- c(localCode, getCompanyCode(cName));
          result = filter(hGL, ref.date == ref.date[index],listed.segment %in% searchTarget);
          if (nrow(result) > 0) {
            localDummyVector <- c(localDummyVector, 1);
          } else {
            localDummyVector <- c(localDummyVector, 0);
          }
        }
      }
      
      yearVector <<- c(yearVector, localYearVector);
      CompanyVector <<- c(CompanyVector, localCompany);
      CodeVector <<- c(CodeVector, localCode);
      DummyVector <<- c(DummyVector, localDummyVector);
    }
  });
  
  resultFrame = data.frame(
    "Código" = CodeVector,
    "Companhia" = CompanyVector,
    "Ano" = yearVector,
    "Dummy" = DummyVector
  );
  
  return(resultFrame);
}

p11(df.statements);
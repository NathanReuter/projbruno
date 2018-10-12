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
name.companies <- allCompanies[10:50];
first.date <- '2010-01-01';
last.date <- '2017-01-01';
df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);

#planilha1 <- p1(processSheet);

# planilha2 <- p2(df.statements);

# planilha3 = p3(df.statements)

#planilha10 <- p10(brunoSheet);

#planilha 5 <- p5(df.statements);

# p7 Nível percentual de ações em posse dos executivos
# Vasculhar history responsable e pegar os person.name com person.job == "Diretor Presidente"
# Depois usar o person.name e em history.stockholders, verificar se person.name == name.stockholder
# e captura o perc.ord.shares
p7 <- function(dataInfo) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  DirectorVector = vector();
  OrderShareVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hResp = company['history.responsible.docs']$history.responsible.docs[[1]];
    if (!is.null(hResp)) {
      hResp = filter(hResp, person.job == "Diretor Presidente");
      hStockHolders = company$history.stockholders[[1]];
      
      if (nrow(hResp) > 0 && nrow(hStockHolders) > 0 ) {
        localYearVector  = vector();
        localCompany = vector();
        localCode = vector();
        localDirector = vector();
        localOrderShare = vector();
        cName = hResp[[1, 1]];
        
        for (name in hResp$person.name) {
          result = filter(hStockHolders, name.stockholder == name);
          #TODO CHECK FOR MORE DE UM RESULT AND CHECK FOR NULL BEFORE FILTER
          if (nrow(result) > 0) {
            for (index in 1:nrow(result)) {
              localDirector <- c(localDirector, name);
              localYearVector <- c(localYearVector, parseDate(result$ref.date[index]));
              localCompany <- c(localCompany, cName);
              localOrderShare <- c(localOrderShare, result$perc.ord.shares[index]);
              localCode <- c(localCode, getCompanyCode(cName));  
            }
          }
        }
        
        yearVector <<- c(yearVector, localYearVector);
        CompanyVector <<- c(CompanyVector, localCompany);
        CodeVector <<- c(CodeVector, localCode);
        DirectorVector <<- c(DirectorVector, localDirector);
        OrderShareVector <<- c(OrderShareVector, localOrderShare);
      }
    }
    
    
  });
  
  resultFrame = data.frame(
    "Codigo" = CodeVector,
    "Compania" = CompanyVector,
    "Ano" = yearVector,
    "Diretor" = DirectorVector,
    "Percentual de Ações" = OrderShareVector
  );
  View(resultFrame)
  return(resultFrame);
}

p7(df.statements)
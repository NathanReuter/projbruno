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

#planilha5 <- p5(df.statements);

#planilha6 <-p6(planilha5);

planilha7 <- p7(df.statements);

#planilha8 <- p8(df.statements);

#p9(df.statements);

#planilha10 <- p10(brunoSheet);

#planilha11 <- p11(df.statements);

#p12(brunoSheet);

#p4 pular
# Usar a 7 para saber quem são os CEO e verficar as pessoas fiísicas em history.stockholders, pegar todos menos o CEO.
# Depois pegar todas s pessoas fisicas e comparar quem faz parte do conselho em history.board.composition
# e somar as ações dessas pessoas

p4 <- function(dataInfo, plan7) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  DirectorVector = vector();
  OrderShareVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hStockHolders = company$history.stockholders[[1]];
    if (!is.null(hStockHolders)) {
      hStockHolders = filter(hStockHolders, type.stockholder == "Fisica");
      
      if (nrow(hStockHolders) > 0) {
        localYearVector  = vector();
        localCompany = vector();
        localCode = vector();
        localDirector = vector();
        localOrderShare = vector();
        cName = hStockHolders[[1, 1]];
        
        for (name in hStockHolders$person.name) {
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
    "Companhia" = CompanyVector,
    "Ano" = yearVector,
    "Diretor" = DirectorVector,
    "Percentual de Ações" = OrderShareVector
  );
  
  return(resultFrame);
}

p4(df.statements, planilha7);
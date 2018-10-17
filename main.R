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
#problematicCompanies = c(239, 212, 197, 179, 161, 199, 205);
problematicCompanies = c("COSAN SA INDUSTRIA E COMERCIO", "COMPANHIA PROVIDENCIA IND E COMERCIO", 
"COBRASMA SA", "CIA ESTADUAL DE GERACAO E TRANSMISSAO DE ENERGIA ELETRICA", "CENTRO DE IMAGEM DIAGN?STICOS S.A.",
"COMPANHIA BRASILEIRA DE DISTRIBUI??O", "COMPANHIA DE SANEAMENTO DE MINAS GERAIS", "INDS J B DUARTE SA",
"CONST SULTEPA SA - EM RECUPERA??O JUDICIAL");

name.companies <- allCompanies[1:10];

name.companies <- setdiff(name.companies, problematicCompanies)
View(name.companies)
first.date <- '2010-01-01';
last.date <- '2017-01-01';
df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);

#planilha1 <- p1(processSheet);

# planilha2 <- p2(df.statements);

# planilha3 = p3(df.statements)

#planilha5 <- p5(df.statements);

#planilha <- p7(df.statements);

#planilha8 <- p8(df.statements);

#planilha10 <- p10(brunoSheet);

#planilha11 <- p11(df.statements);

# p12 Dummy 1 - Setor que será observado; 0 - Caso contrário 
# Separa pela planilha bruno.xls código, nomeCOmpania e nomesetor
p12 <- function(dataInfo) {
  codeVector = vector();
  companyVector = vector();
  activeValueVector = vector();
  yearVector = vector();
  yearCounter <- 2009;
  j <- 0;
  for (colSheet in  brunoSheet[5: length(brunoSheet)]) {
    j = j + length(companyVector)
    yearCounter = yearCounter + 1;
    i = 0;
    k = 3
    for (activeValue in colSheet[k + 1: length(colSheet)]) {
      i = i + 1;
      cName = unlist(brunoSheet[k + i, 2]);
      aValue = as.numeric(unlist(activeValue));
      codeVector[i + j] = getCompanyCode(cName);
      companyVector[i + j] = cName;
      yearVector[i + j] = yearCounter;
      activeValueVector[i + j] = log(aValue);
    }
  }
  
  resultFrame = data.frame("Código" = codeVector, "Companhia" = companyVector, "Ano" = yearVector, 'Valor Ativo' = activeValueVector);
  View(resultFrame)
  
  return (resultFrame)
}

p12(brunoSheet);
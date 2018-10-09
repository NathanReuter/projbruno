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
name.companies <- allCompanies[1:10];
first.date <- '2010-01-01';
last.date <- '2017-01-01';
df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);

#planilha1 <- p1(processSheet);

# planilha2 <- p2(df.statements);

# planilha3 = p3(df.statements)

#planilha10 <- p10(brunoSheet);


# p5 Remuneração Média (RM)
# pegar no history.compensation e dividir total.value / qtd.members
p5 <- function(dataInfo) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  AverageRemunaration = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hComp = company['history.compensation'];
    hComp = hComp$history.compensation[[1]];
    
    
    if (length(hComp) > 0) {
      localYearVector  = vector();
      localCompany = vector();
      localCode = vector();
      localAR = vector();
      localAuxAR = vector();
      cName = hComp[[1, 1]];

      
      for (index in seq_along(hComp$ref.date)) {
        parsedYear = unlist(strsplit(toString(hComp$ref.date[index]), "-"))[1];
        if (!(parsedYear %in% localYearVector)) {
          if (index != 1) {
            localAR <- c(localAR, sum(localAuxAR));
            localAuxAR <- vector();  
          }
          localYearVector <- c(localYearVector, parsedYear);
          localCompany <- c(localCompany, cName);
          localCode <- c(localCode, getCompanyCode(cName));
          
        }
        average = hComp$total.value.remuneration[index]/ hComp$qtd.members[index];
        localAuxAR <- c(localAuxAR, average);
      }
      
      localAR <- c(localAR, sum(localAuxAR));
      yearVector <<- c(yearVector, localYearVector);
      CompanyVector <<- c(CompanyVector, localCompany);
      CodeVector <<- c(CodeVector, localCode);
      AverageRemunaration <<- c(AverageRemunaration, localAR);
    }
  });

  resultFrame = data.frame(
    "Codigo" = CodeVector,
    "Compania" = CompanyVector,
    "Ano" = yearVector,
    "Remuneração Média" = AverageRemunaration
  );
  View(resultFrame)
  return(resultFrame);
}

p5(df.statements)
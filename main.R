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
#problematicCompanies = c(239, 212, 197, 179, 161, 199, 205);
problematicCompanies = c("COSAN SA INDUSTRIA E COMERCIO", "COMPANHIA PROVIDENCIA IND E COMERCIO", 
"COBRASMA SA", "CIA ESTADUAL DE GERACAO E TRANSMISSAO DE ENERGIA ELETRICA", "CENTRO DE IMAGEM DIAGN?STICOS S.A.",
"COMPANHIA BRASILEIRA DE DISTRIBUI??O", "COMPANHIA DE SANEAMENTO DE MINAS GERAIS", "INDS J B DUARTE SA",
"CONST SULTEPA SA - EM RECUPERA??O JUDICIAL");

name.companies <- allCompanies[200:300];

name.companies <- setdiff(name.companies, problematicCompanies)
first.date <- '2010-01-01';
last.date <- '2017-01-01';
df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);

#planilha1 <- p1(processSheet);

# planilha2 <- p2(df.statements);

# planilha3 = p3(df.statements)

#planilha5 <- p5(df.statements);2

#planilha7 <- p7(df.statements);

#planilha8 <- p8(df.statements);

# p9 Tempo de mandato em anos do CEO na empresa
# Olhar history.responsible , verificar todos os person.job = "Diretor Presidente"
# Varrer os anos de 2010 e contar quantos anos de mandato 
# Ano         Diretor:             Tempo:
# 2012        ----                  1
# 2011        Marcos Antonio Molina dos Santos    2
# 2010        Marcos Antonio Molina dos Santos    2
p9 <- function(dataInfo) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  DirectorVector = vector();
  MandateTimeVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hResp = company['history.responsible.docs']$history.responsible.docs[[1]];
    if (!is.null(hResp)) {
      hResp = filter(hResp, person.job %in% c("Diretor Presidente", "Diretor Presidente/Relações com Investidores"));
      
      if (nrow(hResp) > 0) {
        localYearVector  = vector();
        localCompany = vector();
        localCode = vector();
        localDirector = vector();
        localMandateTime = vector();
        cName = hResp[[1, 1]];
        actualDirectorName = "";
        yearsCounter = 0;
        for (index in 1:nrow(hResp)) {
          if (!is.null(hResp$person.name[index]) && !is.na(hResp$person.name[index])) {
            if (actualDirectorName == "" || actualDirectorName == hResp$person.name[index]) {
              yearsCounter = yearsCounter + 1;
            } else {
              localMandateTime <- c(localMandateTime, rep(c(yearsCounter), yearsCounter));
              yearsCounter = 1;
            }
            
            actualDirectorName = hResp$person.name[index];
            localDirector <- c(localDirector, hResp$person.name[index]);
            localYearVector <- c(localYearVector, parseDate(hResp$ref.date[index]));
            localCompany <- c(localCompany, cName);
            localMandateTime <- c(localMandateTime, hResp$perc.ord.shares[index]);
            localCode <- c(localCode, getCompanyCode(cName));  
          }
        }
        localMandateTime <- c(localMandateTime, rep(c(yearsCounter), yearsCounter));
        yearVector <<- c(yearVector, localYearVector);
        CompanyVector <<- c(CompanyVector, localCompany);
        CodeVector <<- c(CodeVector, localCode);
        DirectorVector <<- c(DirectorVector, localDirector);
        MandateTimeVector <<- c(MandateTimeVector, localMandateTime);
      }
    }
    
    
  });
  
  resultFrame = data.frame(
    "Codigo" = CodeVector,
    "Companhia" = CompanyVector,
    "Ano" = yearVector,
    "Diretor" = DirectorVector,
    "Tempo de Mandato (Anos)" = MandateTimeVector
  );
  View(resultFrame);
  
  return(resultFrame);
}

p9(df.statements);
#planilha10 <- p10(brunoSheet);

#planilha11 <- p11(df.statements);

#p12(brunoSheet);
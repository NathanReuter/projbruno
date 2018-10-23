setwd("~/projects/projetoBruno/progFiles")
#setwd("~/projbruno")
# Load local Scripts
source("utils.R");
source("sheetFunctions.R");
# Load GetDFP library
library("GetDFPData");
library("dplyr")

# # Load needed csv
brunoSheet = read_excel("resourceSheets/Bruno.xlsx");
# processSheet = read_excel_allsheets("resourceSheets/processos.xlsx");
# processSheet = processSheet[7: length(processSheet)-1];
# # Get all companies name, this will be use later
# allCompanies = gdfpd.get.info.companies(type.data = "companies")[[1]];
# # Test info to get ONE companie info
# 
# name.companies <- allCompanies;
# name.companies <- setdiff(name.companies, c("MARAMBAIA ENERGIA RENOVÁVEL SA"));
# first.date <- '2010-01-01';
# last.date <- '2017-01-01';
# df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);
codeAndName = read.csv("./codeAndName.csv"); 
fullSheet = function() {
  PJVector = vector();
  MCVector = vector();
  SDVector = vector();
  FCVector = vector();
  PCVector = vector();
  APCVector = vector();
  RMVector = vector();
  RMRankit = vector()
  RMM = vector();
  POEDIRECTOR = vector();
  POEPercent = vector();
  POCVector = vector();
  AMDirector = vector();
  AMTime = vector();
  SizeVector = vector();
  SETVector = vector();
  
  for (index in 1:nrow(planilha11)) {
    actualCode <- planilha11$Código[index];
    actualYear <- planilha11$Ano[index];
    
    PJ <- filter(planilha1, planilha1$Código == actualCode & planilha1$Ano == actualYear );
    
    if (nrow(PRV) > 0) {
      PJVector <- c(PJVector, PJ$Número.Processos[1]);
    } else {
      PJVector <- c(PJVector, "");
    }
    
    PRV <- filter(planilha2, planilha2$Codigo == actualCode & planilha2$Ano == actualYear );
    
    if (nrow(PRV) > 0) {
      MCVector <- c(MCVector, PRV$Management.Council[1]);
      SDVector <- c(SDVector, PRV$Statutory.Directors[1]);
      FCVector <- c(FCVector, PRV$Fiscal.Council[1]);
    } else {
      MCVector <- c(MCVector, 0);
      SDVector <- c(SDVector, 0);
      FCVector <- c(FCVector, 0);
    }
    
    PC <- filter(planilha3, planilha3$Codigo == actualCode & planilha3$Ano == actualYear );
    
    if (nrow(PC) > 0) {
      PCVector <- c(PCVector, round(PC$Porcentagem.Conselheiros[1], 2));
    } else {
      PCVector <- c(PCVector, 0);
    }
    
    APC <- filter(planilha4, planilha4$Codigo == actualCode & planilha4$Ano == actualYear );
    
    if (nrow(APC) > 0) {
      APCVector <- c(APCVector, APC$APC[1]);
    } else {
      APCVector <- c(APCVector, 0);
    }
    
    RMdata <- filter(planilha6, planilha6$Codigo == actualCode & planilha6$Ano == actualYear );
    if (nrow(RMdata) > 0) {
      RMVector <- c(RMVector, round(RMdata$Remuneração.Média[1], 2));
      RMRankit <- c(RMRankit, RMdata$RMRankit[1]);
      RMM <- c(RMM, RMdata$RMM[1]);
    } else {
      RMVector <- c(RMVector, 0);
      RMRankit <- c(RMRankit, 0);
      RMM <- c(RMM, 0);
    }
    
    POE <- filter(planilha7, planilha7$Codigo == actualCode & planilha7$Ano == actualYear );
    
    if (nrow(POE) > 0) {
      directorValue = POE$Diretor[1];
      if (typeof(directorValue) != "character") {
        directorValue = levels(directorValue)[directorValue];
      }
      POEDIRECTOR <- c(POEDIRECTOR, directorValue);
      POEPercent <- c(POEPercent, round(POE$Percentual.de.Ações[1], 2));
    } else {
      POEDIRECTOR <- c(POEDIRECTOR, "");
      POEPercent <- c(POEPercent, 0);
    }
    
    POC <- filter(planilha8, planilha8$Código == actualCode & planilha8$Ano == actualYear );
    
    if (nrow(POC) > 0) {
      POCVector <- c(POCVector, POC$Dummy[1]);
    } else {
      POCVector <- c(POCVector, 0);
    }
    
    AM <- filter(planilha9, planilha9$Codigo == actualCode & planilha9$Ano == actualYear );
    
    if (nrow(AM) > 0) {
      directorValue = AM$Diretor[1];
      if (typeof(directorValue) != "character") {
        directorValue = levels(directorValue)[directorValue];
      }
      AMDirector <- c(AMDirector, directorValue);
      AMTime <- c(AMTime, AM$Tempo.de.Mandato..Anos.[1]);
    } else {
      AMDirector <- c(AMDirector, "");
      AMTime <- c(AMTime, 0);
    }
    
    SIZE <- filter(planilha10, planilha10$Código == actualCode & planilha10$Ano == actualYear )
    
    if (nrow(SIZE) > 0) {
      SizeVector <- c(SizeVector, round(SIZE$Valor.Ativo[1], 2));
    } else {
      SizeVector <- c(SizeVector, 0);
    }
    
    SET <- filter(planilha12, planilha12$Código == actualCode & planilha12$Ano == actualYear )
    
    if (nrow(SET) > 0) {
      setor = SET$Valor.Ativo[1];
      if (typeof(setor) != "character") {
        setor = levels(setor)[setor];
      }
      SETVector <- c(SETVector, setor);
    } else {
      SETVector <- c(SETVector, "");
    }
    
  }
  
  planilhaCompleta <- planilha11;
  colnames(planilhaCompleta)[5] = "NIV"
  planilhaCompleta["PJ"] = PJVector;
  planilhaCompleta["Management.Council"] = MCVector;
  planilhaCompleta["Statutory.Directors"] = SDVector;
  planilhaCompleta["Fiscal.Council"] = FCVector;
  planilhaCompleta["PC"] = PCVector;
  planilhaCompleta["APC"] = APCVector;
  planilhaCompleta["RM"] = RMVector;
  planilhaCompleta["RMRANKIT"] = RMRankit;
  planilhaCompleta["RMM"] = RMM;
  planilhaCompleta["POEPercent"] = POEPercent;
  planilhaCompleta["AM"] = AMTime;
  planilhaCompleta["PO"] = POCVector;
  planilhaCompleta["AMDIRECTOR"] = AMDirector;
  planilhaCompleta["AM"] = AMTime;
  planilhaCompleta["SIZE"] = SizeVector;
  planilhaCompleta["SETOR"] = SETVector;
  
  View(planilhaCompleta);
  
  return(planilhaCompleta)
}
planilhaCompleta <- fullSheet();
AIAE1 = vector();
AIAE2 = vector();
PE = vector();
for (index in 1:nrow(planilhaCompleta)) {
  value = sum(planilhaCompleta$Management.Council[index],
              planilhaCompleta$Statutory.Directors[index],
              planilhaCompleta$Fiscal.Council[index],
              planilhaCompleta$PC[index],
              planilhaCompleta$APC[index]);
  vale = value - as.numeric(planilhaCompleta$PJ[index]);
  AIAE1 <- c(AIAE1, value);
  value = sum(planilhaCompleta$Management.Council[index],
              planilhaCompleta$Statutory.Directors[index],
              planilhaCompleta$Fiscal.Council[index],
              planilhaCompleta$APC[index]);
  value = value -as.numeric(planilhaCompleta$PJ[index]);
  AIAE2 = c(AIAE2, value);
  value = sum(planilhaCompleta$POEPercent[index],
              planilhaCompleta$PO[index],
              planilhaCompleta$AM[index]) ;
  PE = c(PE, round(value, 2));
  
}
planilhaCompleta["AIAE1"] = AIAE1;
planilhaCompleta["AIAE2"] = AIAE2;
planilhaCompleta["PE"] = PE;
View(planilhaCompleta);
write.xlsx2(planilhaCompleta, "./excels/PlanilhaCompleta.xlsx");



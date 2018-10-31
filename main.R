setwd("~/projects/projetoBruno/progFiles")
#setwd("~/projbruno")
# Load local Scripts
source("utils.R");
source("sheetFunctions.R");
source("newsheets.r");
# Load GetDFP library
library("GetDFPData");
library("dplyr")

# # Load needed csv
brunoSheet = read_excel("resourceSheets/Bruno.xlsx");
processSheet = read_excel_allsheets("resourceSheets/processos.xlsx");
processSheet = processSheet[7: length(processSheet)-1];
codeAndName = read.csv("./codeAndName.csv");
try({is.null(df.statements)}, {load("./saves/statements.Rda");})
planilha1 = read.csv("./completas/Planilha 1.csv");
#RPJ = loadData("RPJ");
#QT = loadData("QT")
#RQT = loadData("RQT");
#P7 = read.csv("./completas/Planilha 7.csv");
#APC = loadData("APC");
#RAPC = loadData("RAPC")
#PC = loadData("PC")
#RPC = loadData("RPC")
#RE = loadData("RE");
#RRE = loadData("RRE");
#PRV = loadData("PRV");
#RPRVFunc(PRV) = loadData("RPRV");
#POE = loadData("POE");
#RPOE = RPOEFunc(POE);
#POCPE = loadData("POCPE");
#AM = loadData("AM");
#RMA = loadData("RMA");
#SIZE = loadData("SIZE");
NIVFunc <- function(dataInfo) {
  resultFrame= data.frame();
  isNiv <- function(X) {
    if (X %in% c("Novo Mercado", "Nível 2")) {
      return(1);
    }
       
    return (0)
  }
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    
    try({
      cCode = selectedInfo$company.code;
      cName = selectedInfo$company.name;
      hb =selectedInfo$history.governance.listings[[1]];
      NIV = sapply(hb$listed.segment, isNiv);
      hb["NIV"] = NIV;
      hb["Código"] = cCode;
      resultFrame = rbind(resultFrame, hb[, c(1,2,6, 7)]);  
    });
  }
  
  resultFrame$ref.date = sapply(resultFrame$ref.date, parseDate);
  colnames(resultFrame)[1] = "Compahnia";
  colnames(resultFrame)[2] = "Ano";
  resultFrame = do.call(data.frame,lapply(resultFrame, function(x) replace(x, is.infinite(x), 0)))
  View(resultFrame);
  
  saveData(resultFrame, "NIV");
  
  return (resultFrame);
}


NIV = NIVFunc(df.statements);





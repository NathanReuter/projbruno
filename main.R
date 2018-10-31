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
#load("./saves/rpj.Rda")
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


















POEFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    try({
      cName = selectedInfo$company.name;
      hb =selectedInfo$history.responsible.docs[[1]];
      hs =selectedInfo$history.stockholders[[1]];
      hb = hb[hb$person.job == "Diretor Presidente",];
      filtered = filter(hs, name.stockholder %in% hb$person.name);
      if (nrow(filtered) > 0) {
        POE = round(as.numeric(filtered$perc.ord.shares) + as.numeric(filtered$perc.pref.shares), 2)
        filtered["POE"] = POE;
        filtered["Código"] = cCode;
        resultFrame = rbind(resultFrame, filtered[, c(1,2,15, 16)]);  
      }
        
    })
  }
  resultFrame$ref.date = sapply(resultFrame$ref.date, parseDate);
  colnames(resultFrame)[1] = "Compahnia";
  colnames(resultFrame)[2] = "Ano";
  resultFrame = do.call(data.frame,lapply(resultFrame, function(x) replace(x, is.infinite(x), 0)))
  View(resultFrame);
  
  saveData(resultFrame, "POE");
  
  return (resultFrame);
}

POE = POEFunc(df.statements)
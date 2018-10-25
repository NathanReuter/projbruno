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
load("./saves/rpj.Rda")
QT = loadData("QT")

#Planilha 4: Ranking Q de Tobin Simplificado (RQT)
#RQTit = (RankRQTit – 1)/(N – 1).
QTFun <- function(dataInfo) {
  resultFrame = dataInfo[, c(1,2)];
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code
    mktvalue = selectedInfo$history.mkt.value[[1]];
    valorMercado = mktvalue[, c(2,3)];
    liabilities = selectedInfo$fr.liabilities[[1]]
    passivoTotal = liabilities[liabilities$acc.desc == "Passivo Total", c(2,5)]
    passivoTotal$passivoTotal = passivoTotal$acc.value * 1000;
    patrimonioLiquido = liabilities[liabilities$acc.desc == "Patrimônio Líquido", c(2,5)];
    patrimonioLiquido$patrimonioLiquido = patrimonioLiquido$acc.value * 1000;
    assets = selectedInfo$fr.assets[[1]];
    ativoTotal = assets[assets$acc.desc == "Ativo Total", c(2,5)];
    ativoTotal$ativoTotal = ativoTotal$acc.value * 1000;
    mergedData = Reduce(function(x, y) merge(x, y, by = "ref.date"), list(valorMercado, passivoTotal, patrimonioLiquido, ativoTotal));
    QTvalue = (mergedData$mkt.avg.value + (mergedData$passivoTotal - mergedData$patrimonioLiquido)) / mergedData$ativoTotal;
    mergedData = merge(mergedData, rep(cCode, nrow(mergedData)), all=TRUE);
    colnames(mergedData)[9] = "company.code";
    mergedData = merge(mergedData, QTvalue, all=TRUE)[, c(1, 9, 10)];
    colnames(mergedData)[3] = "QT";
    resultFrame = merge(resultFrame, mergedData,  by = "company.code");  
  }
  
  View(resultFrame);
  saveData(resultFrame, "QT");
  return (resultFrame);
  # sapply(resultFrame$ref.date, parseDate) for format date in all vector
}

QTFun(df.statements);
# RQT <- function(QT) {
#   sorted = QT[order(-QT$QT),];
#   years = unique(QT$ref.date);
#   for (year in years) {
#     filtered = filter(sorted, ref.date == year);
#     n = nrow(filtered);
#   }
#  View(sorted) 
# }
# 
# 
# RQT(QT);

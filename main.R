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
#Q de Tobin Simplificado (QT)
#[Valor de Mercado + (Passivo Total – Patrimônio Líquido)]/Ativo Total
QT <- function(dataInfo) {
  mktvalue = dataInfo$history.mkt.value[[1]];
  valorMercado = mktvalue[, c(2,3)];
  liabilities = dataInfo$fr.liabilities[[1]]
  passivoTotal = liabilities[liabilities$acc.desc == "Passivo Total", c(2,5)]
  passivoTotal$passivoTotal = passivoTotal$acc.value * 1000;
  patrimonioLiquido = liabilities[liabilities$acc.desc == "Patrimônio Líquido", c(2,5)];
  patrimonioLiquido$patrimonioLiquido = patrimonioLiquido$acc.value * 1000;
  assets = dataInfo$fr.assets[[1]];
  ativoTotal = assets[assets$acc.desc == "Ativo Total", c(2,5)];
  ativoTotal$ativoTotal = ativoTotal$acc.value * 1000;
  mergedData = Reduce(function(x, y) merge(x, y, by = "ref.date"), list(valorMercado, passivoTotal, patrimonioLiquido, ativoTotal));
  QTvalue = (mergedData$mkt.avg.value + (mergedData$passivoTotal - mergedData$patrimonioLiquido)) / mergedData$ativoTotal;
  mergedData = merge(mergedData, QTvalue, all=TRUE)[, c(1, 9)];
  colnames(mergedData)[2] = "QT";
  resultFrame = merge(dataInfo[, c(1,2)], mergedData);
  View(resultFrame);
  saveData(resultFrame, "QT");
  return (resultFrame);
  # sapply(resultFrame$ref.date, parseDate) for format date in all vector
}

QTFile = QT(df.statements);
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
#planilha1 = read.csv("./completas/Planilha 1.csv");
PJ = loadData("PJ");
RPJ = loadData("RPJ");
QT = loadData("QT")
RQT = loadData("RQT");
APC = loadData("APC");
RAPC = loadData("RAPC")
PC = loadData("PC")
RPC = loadData("RPC")
RE = loadData("RE");
RRE = loadData("RRE");
PRV = loadData("PRV");
RPRV = loadData("RPRV");
POE = loadData("POE");
RPOE = loadData("RPOE");
POCPE = loadData("POCPE");
AM = loadData("AM");
RMA = loadData("RMA");
SIZE = loadData("SIZE");
NIV = loadData("NIV");

findCode = function (name) {
  return (codeAndName[codeAndName$company.name == name]$company.code)
}

completaFunc = function(RPJ, RQT, RAPC, RPC, RRE, RPRV, RPOE, POCPE, RMA, SIZE, NIV) {
  merge1 = merge(RPJ, RQT, all = TRUE);
  #merge2 = merge(merge1, RAPC, all = TRUE);
  merge2 = merge1;
  merge3 = merge(merge2, RPC, all = TRUE);
  merge4 = merge(merge3, RRE, all = TRUE);
  merge5 = merge(merge4, RPRV, all = TRUE);
  merge6 = merge(merge5, RPOE, all = TRUE);
  merge7 = merge6;
  #merge7 = merge(merge6, POCPE, all = TRUE);
  #merge8 = merge(merge7, RMA, all = TRUE);
  merge8 = merge7;
  merge9 = merge(merge8, SIZE, all = TRUE);
  merge10 = merge(merge9, NIV, all = TRUE);
  completa = merge10;
  completa["AIAE1"] = completa$RQT - completa10$RPJ;
  #["AIAE2"] = completa$RQT - completa10$RPJ;
}







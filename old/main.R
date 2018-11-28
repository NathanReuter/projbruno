setwd("~/projects/projetoBruno/progFiles")
#setwd("~/projbruno")
# Load local Scripts
source("utils.R");
source("sheetFunctions.R");
source("newsheets.r");
# Load GetDFP library
library("GetDFPData");
library("dplyr") 
library(httr)
set_config(config(ssl_verifypeer = 0L))

# # Load needed csv
brunoSheet = read_excel("resourceSheets/Bruno.xlsx");
processSheet = read_excel_allsheets("resourceSheets/processos.xlsx");
processSheet = processSheet[7: length(processSheet)-1];
codeAndName = read.csv("./codeAndName.csv");
#try({is.null(df.statements)}, {load("./saves/statements.Rda");})
#planilha1 = read.csv("./completas/Planilha 1.csv");
# PJ = loadData("PJ");
# RPJ = loadData("RPJ");
# QT = loadData("QT")
# RQT = loadData("RQT");
# APC = loadData("APC");
# RAPC = loadData("RAPC")
# PC = loadData("PC")
# RPC = loadData("RPC")
# RE = loadData("RE");
# RRE = loadData("RRE");
# PRV = loadData("PRV");
# RPRV = loadData("RPRV");
# POE = loadData("POE");
# RPOE = loadData("RPOE");
# POCPE = loadData("POCPE");
# AM = loadData("AM");
# RMA = loadData("RMA");
# SIZE = loadData("SIZE");
# NIV = loadData("NIV");
# Test info to get ONE companie info
load("./mydata.RData");
# ROEFunc <- function(dataInfo) {
#   resultFrame= data.frame();
#   for (index in 1:nrow(dataInfo)) {
#     selectedInfo = dataInfo[index, ]
#     cCode = selectedInfo$company.code;
#     cName = selectedInfo$company.name;
#     hb =selectedInfo$fr.cashflow[[1]];
#     lucroLiquido = hb[hb$acc.desc == "Lucro líquido do exercício", ]$acc.value;
#     patrimonioLiquido = hb[hb$acc.desc == "Patrimônio Líquido", ]$acc.value
#   
#     if (length(lucroLiquido) && length(patrimonioLiquido)) {
#       value = lucroLiquido / patrimonioLiquido;
#       resultFrame = rbind(resultFrame, parcialFrame);  
#     }
#     
#   }
#   
#   View(resultFrame);
#   
#   saveData(resultFrame, "PC");
#   
#   return (resultFrame);
# }

# ROEFunc(df.statements);

#name.companies <- "LOCALIZA RENT A CAR SA";
#name.companies <- setdiff(name.companies, c("MARAMBAIA ENERGIA RENOVÁVEL SA"));
#first.date <- '2010-01-01';
#last.date <- '2017-01-01';
#statement <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date, fetch.new.files = TRUE, do.cache = TRUE, max.dl.tries = 10);


CFunc <- function(dataInfo) {
  resultFrame= data.frame();
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb =selectedInfo$fr.cashflow[[1]];
    hasPatri = hb[hb$acc.number == "6.01.01.01", ];
    if (nrow(hasPatri) > 0) {
      print("FIND!");
    }
  }
}

CFunc(df.statements);

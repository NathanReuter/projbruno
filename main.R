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
PRV = loadData("PRV");
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

df.statements = data.frame();
allCompanies = loadData("allCompanies");
doneCompanies = loadData("doneCompanies");
percent = length(doneCompanies)*100 / length(allCompanies);
print(percent)
for (company in setdiff(allCompanies, doneCompanies)) {
  try({
    name.companies <- company;
    #name.companies <- setdiff(name.companies, c("MARAMBAIA ENERGIA RENOVÁVEL SA"));
    first.date <- '2010-01-01';
    last.date <- '2017-01-01';
    statement <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);
    df.statements <- c(df.statements, statement);
    doneCompanies <- c(doneCompanies, company);
    saveData(doneCompanies, "doneCompanies");
  });  
}
# Test info to get ONE companie info

#save(df.statements,file="statements.Rda")



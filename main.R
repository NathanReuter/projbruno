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
#allCompanies = gdfpd.get.info.companies(type.data = "companies")[[1]];
# Test info to get ONE companie info

#name.companies <- allCompanies;
name.companies <- setdiff(name.companies, c("MARAMBAIA ENERGIA RENOVÁVEL SA"));
first.date <- '2010-01-01';
last.date <- '2017-01-01';
#df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);
codeAndName = read.csv("./codeAndName.csv"); 

#planilha1 <- p1(processSheet);

#planilha2 <- p2(df.statements);

#planilha3 <- p3(df.statements)

#planilha5 <- p5(df.statements);

#planilha6 <-p6(planilha5);

#planilha7 <- p7(df.statements);

#planilha8 <- p8(df.statements);

#planilha9 <- p9(df.statements);

#planilha10 <- p10(brunoSheet);

#planilha11 <- p11(df.statements);

planilha12 <- p12(brunoSheet);

#planilha4 <- p4(df.statements, planilha7);



write.csv(planilha12, file = "./Planilha 12.csv");
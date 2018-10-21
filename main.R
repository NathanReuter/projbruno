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
allCompanies = gdfpd.get.info.companies(type.data = "companies")[[1]];
# Test info to get ONE companie info

name.companies <- allCompanies;
name.companies <- setdiff(name.companies, c("MARAMBAIA ENERGIA RENOVÁVEL SA"));
first.date <- '2010-01-01';
last.date <- '2017-01-01';
df.statements <- gdfpd.GetDFPData(name.companies = name.companies,first.date = first.date);
codeAndName = read.csv("./codeAndName.csv"); 

#planilha1 <- p1(processSheet);

#planilha2 <- p2(df.statements);
#View(planilha2)
try({
  planilha3 <- p3(df.statements)
  View(planilha3)
  write.csv(planilha3, file = "./Planilha 3.csv");
})
try({
  planilha5 <- p5(df.statements);
  View(planilha5)
  write.csv(planilha5, file = "./Planilha 5.csv");
  planilha6 <-p6(planilha5);
  View(planilha6);
  write.csv(planilha6, file = "./Planilha 6.csv");
})
try({
  planilha7 <- p7(df.statements);
  View(planilha7);
  write.csv(planilha7, file = "./Planilha 7.csv");
  planilha4 <- p4(df.statements, planilha7);
  View(planilha4);
  write.csv(planilha4, file = "./Planilha 4.csv");
})
try({
  planilha8 <- p8(df.statements);
  View(planilha8)
  write.csv(planilha8, file = "./Planilha 8.csv");
})
try({
  planilha9 <- p9(df.statements);
  View(planilha9)
  write.csv(planilha9, file = "./Planilha 9.csv");
})
try({
  planilha11 <- p11(df.statements);
  View(planilha11);
  write.csv(planilha11, file = "./Planilha 11.csv");
})


library("dplyr");
source("utils.R");
allData = readRDS("./data/allData.rds");

#Indicador do Preço/Lucro (PL)
PLFunc <- function(dataInfo) {
  resultFrame= data.frame();
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb = selectedInfo$history.mkt.value[[1]];
    hb2 = selectedInfo$history.stockholders[[1]]
    hb2 = hb2[hb2$type.register == "Total", ];
    years = sapply(unique(hb$ref.date), parseDate);
    valorDeMercado = hb$mkt.avg.value;
    numeroAcoesEmpresa = as.numeric(hb2$qtd.ord.shares) + as.numeric(hb2$qtd.pref.shares);
    PL = valorDeMercado/numeroAcoesEmpresa;
    partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "PL"=PL);
    resultFrame = rbind(resultFrame, partialFrame)
  }
  
  View(resultFrame);
  
  saveData(resultFrame, "PL");
  
  return (resultFrame);
}
#PLFunc(allData);
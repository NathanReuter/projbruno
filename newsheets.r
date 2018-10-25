#Ranking dos Processos Judiciais (RPJ)
rpjFunc <- function(dataInfo) {
  sorted = dataInfo[order(-dataInfo$Número.Processos),]
  years = c(2010,2011,2012,2013,2014,2015,2016,2017);
  codeVector  = vector();
  yearVector  = vector();
  rpjVector = vector();
  
  for (year in years) {
    filtered = filter(sorted, Ano == year);
    numberR = nrow(filtered)
    if (numberR > 0) {
      for (index in 1:numberR) {
        codeVector <- c(codeVector, filtered$Código[index]);
        yearVector <- c(yearVector, filtered$Ano[index]);
        rpjValue <-  ((numberR - index)) / (numberR);
        rpjVector <- c(rpjVector, rpjValue);
      }
    }
  }
  
  semiResultFrame = data.frame(
    "Código" = codeVector,
    "Ano" = yearVector,
    "RPJ" = rpjVector
  );
  
  return (merge(dataInfo ,semiResultFrame, by=c("Código","Ano")))
}

#Q de Tobin Simplificado (QT)
#[Valor de Mercado + (Passivo Total – Patrimônio Líquido)]/Ativo Total
QTFun <- function(dataInfo) {
  
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
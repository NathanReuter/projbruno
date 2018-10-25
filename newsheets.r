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
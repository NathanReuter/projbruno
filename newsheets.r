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

#Planilha 4: Ranking Q de Tobin Simplificado (RQT)
#RQTit = (RankRQTit – 1)/(N – 1).
QTFun <- function(dataInfo) {
  resultFrame= data.frame();
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    percent = 100 -((nrow(dataInfo) - index) * 100/nrow(dataInfo));
    print(percent);
    mktvalue = selectedInfo$history.mkt.value[[1]];
    valorMercado = mktvalue[, c(2,3)];
    liabilities = selectedInfo$fr.liabilities[[1]]
    passivoTotal = liabilities[liabilities$acc.desc == "Passivo Total", c(2,5)]
    passivoTotal$passivoTotal = passivoTotal$acc.value * 1000;
    patrimonioLiquido = liabilities[liabilities$acc.desc == "Patrimônio Líquido", c(2,5)];
    patrimonioLiquido$patrimonioLiquido = patrimonioLiquido$acc.value * 1000;
    assets = selectedInfo$fr.assets[[1]];
    ativoTotal = assets[assets$acc.desc == "Ativo Total", c(2,5)];
    ativoTotal$ativoTotal = ativoTotal$acc.value * 1000;
    try({
      mergedData = Reduce(function(x, y) merge(x, y, by = "ref.date"), list(valorMercado, passivoTotal, patrimonioLiquido, ativoTotal));
      QTvalue = (mergedData$mkt.avg.value + (mergedData$passivoTotal - mergedData$patrimonioLiquido)) / mergedData$ativoTotal;
      mergedData["Código"] = rep(cCode, nrow(mergedData));
      mergedData["Nome"] = rep(cName, nrow(mergedData));
      mergedData["QT"] = QTvalue;
      mergedData = mergedData[, c(1, 9, 10, 11)];
      resultFrame = rbind(resultFrame, mergedData);  
    })
  }
  
  resultFrame$ref.date = sapply(resultFrame$ref.date, parseDate);
  colnames(resultFrame)[1] = "Ano";
  View(resultFrame);
  
  saveData(resultFrame, "QT");
  return (resultFrame);
}

# Ranking Q de Tobin Simplificado (RQT)
RQTFunc <- function(QT) {
  sorted = QT[order(-QT$QT),];
  years = unique(QT$Ano);
  resultFrame = data.frame();
  for (year in years) {
    filtered = filter(sorted, Ano == year);
    nElements = nrow(filtered);
    Rank =  nElements - as.numeric(rownames(filtered));
    RQTit = (Rank)/(nElements - 1)
    filtered["RQT"] = RQTit;
    resultFrame = rbind(resultFrame, filtered);
  }
  
  View(resultFrame);
  saveData(resultFrame, "RQT");
  return (resultFrame);
}

RAPCFunc <- function(APC) {
  sorted = APC[order(-APC$APC),];
  years = unique(QT$Ano);
  resultFrame = data.frame();
  for (year in years) {
    filtered = filter(sorted, Ano == year);
    nElements = nrow(filtered);
    Rank =  nElements - as.numeric(rownames(filtered));
    RQTit = (Rank)/(nElements - 1)
    filtered["RAPC"] = RQTit;
    resultFrame = rbind(resultFrame, filtered);
  }
  
  View(resultFrame);
  saveData(resultFrame, "RAPC");
  
  return (resultFrame);
}

PCFunc <- function(dataInfo) {
  resultFrame= data.frame();
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb =selectedInfo$history.board.composition[[1]];
    years = unique(hb$ref.date);
    for (year in years) {
      filtered = hb[hb$ref.date == year, ];
      total = nrow(unique(filtered));
      count = nrow(filtered[filtered$code.type.job == 27, ]);
      value = round(count/total, 4) * 100;
      parcialFrame = data.frame("Compahnia"=cName, "Código"=cCode, "Ano"=parseDate(filtered$ref.date[1]), "PC"=value);
      resultFrame = rbind(resultFrame, parcialFrame);
    }
  }
  
  View(resultFrame);
  
  saveData(resultFrame, "PC");
  
  return (resultFrame);
}

RPCFunc <- function(PC) {
  sorted = PC[order(-PC$PC),];
  years = unique(PC$Ano);
  resultFrame = data.frame();
  for (year in years) {
    filtered = filter(sorted, Ano == year);
    nElements = nrow(filtered);
    Rank =  nElements - as.numeric(rownames(filtered));
    RQTit = (Rank)/(nElements - 1)
    filtered["RPC"] = RQTit;
    resultFrame = rbind(resultFrame, filtered);
  }
  
  View(resultFrame);
  saveData(resultFrame, "RPC");
  
  return (resultFrame);
}

RRECFunc <- function(DATA) {
  sorted = DATA[order(-DATA$RE),];
  years = unique(DATA$Ano);
  resultFrame = data.frame();
  for (year in years) {
    filtered = filter(sorted, Ano == year);
    nElements = nrow(filtered);
    Rank =  nElements - as.numeric(rownames(filtered));
    RQTit = (Rank)/(nElements - 1)
    filtered["RRE"] = RQTit;
    resultFrame = rbind(resultFrame, filtered);
  }
  
  View(resultFrame);
  saveData(resultFrame, "RRE");
  
  return (resultFrame);
}

PRVFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    try({
      cName = selectedInfo$company.name;
      hb =selectedInfo$history.compensation[[1]];
      hb = hb[hb$level.remuneration == "Statutory Directors",];
      PRV = (hb$variable.bonus + hb$variable.results.participation + hb$variable.meetings.participation 
             + hb$variable.commissions.participation + hb$variable.others 
             + hb$stocks.options.benefits) / hb$total.value.remuneration;
      hb["PRV"] = round(PRV, 4) * 100;
      hb["Código"] = cCode;
      parcialFrame = hb[, c(1,2,19,20)];
      resultFrame = rbind(resultFrame, parcialFrame);  
    })
  }
  resultFrame$ref.date = sapply(resultFrame$ref.date, parseDate);
  colnames(resultFrame)[1] = "Compahnia";
  colnames(resultFrame)[2] = "Ano";
  resultFrame = do.call(data.frame,lapply(resultFrame, function(x) replace(x, is.infinite(x), 0)))
  View(resultFrame);
  
  saveData(resultFrame, "PRV");
  
  return (resultFrame);
}

RPRVFunc <- function(DATA) {
  sorted = DATA[order(-DATA$PRV),];
  years = unique(DATA$Ano);
  resultFrame = data.frame();
  for (year in years) {
    filtered = filter(sorted, Ano == year);
    nElements = nrow(filtered);
    Rank =  nElements - as.numeric(rownames(filtered));
    RQTit = (Rank)/(nElements - 1)
    filtered["RPRV"] = RQTit;
    resultFrame = rbind(resultFrame, filtered);
  }
  
  View(resultFrame);
  saveData(resultFrame, "RPRV");
  
  return (resultFrame);
}

POEFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    try({
      cName = selectedInfo$company.name;
      hb =selectedInfo$history.responsible.docs[[1]];
      hs =selectedInfo$history.stockholders[[1]];
      hb = hb[hb$person.job == "Diretor Presidente",];
      filtered = filter(hs, name.stockholder %in% hb$person.name);
      if (nrow(filtered) > 0) {
        POE = round(as.numeric(filtered$perc.ord.shares) + as.numeric(filtered$perc.pref.shares), 2)
        filtered["POE"] = POE;
        filtered["Código"] = cCode;
        resultFrame = rbind(resultFrame, filtered[, c(1,2,15, 16)]);  
      }
      
    })
  }
  resultFrame$ref.date = sapply(resultFrame$ref.date, parseDate);
  colnames(resultFrame)[1] = "Compahnia";
  colnames(resultFrame)[2] = "Ano";
  resultFrame = do.call(data.frame,lapply(resultFrame, function(x) replace(x, is.infinite(x), 0)))
  View(resultFrame);
  
  saveData(resultFrame, "POE");
  
  return (resultFrame);
}

RPOEFunc <- function(DATA) {
  sorted = DATA[order(-DATA$POE),];
  years = unique(DATA$Ano);
  resultFrame = data.frame();
  for (year in years) {
    filtered = filter(sorted, Ano == year);
    nElements = nrow(filtered);
    Rank =  nElements - as.numeric(rownames(filtered));
    RQTit = (Rank)/(nElements - 1)
    filtered["RPOE"] = RQTit;
    resultFrame = rbind(resultFrame, filtered);
  }
  
  View(resultFrame);
  saveData(resultFrame, "POE");
  
  return (resultFrame);
}

RMAFunc <- function(DATA) {
  sorted = DATA[order(-DATA$AM),];
  years = unique(DATA$Ano);
  resultFrame = data.frame();
  for (year in years) {
    filtered = filter(sorted, Ano == year);
    nElements = nrow(filtered);
    Rank =  nElements - as.numeric(rownames(filtered));
    RQTit = (Rank)/(nElements - 1)
    filtered["RMA"] = RQTit;
    resultFrame = rbind(resultFrame, filtered);
  }
  
  View(resultFrame);
  saveData(resultFrame, "RMA");
  
  return (resultFrame);
}

SIZEFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    
    try({
      cCode = selectedInfo$company.code;
      cName = selectedInfo$company.name;
      hb =selectedInfo$fr.assets[[1]];
      hb = hb[hb$acc.desc == "Ativo Total",];
      SIZE = log(hb$acc.value * 1000);
      hb["SIZE"] = SIZE;
      hb["Código"] = cCode;
      resultFrame = rbind(resultFrame, hb[, c(1,2,7, 8)]);  
    });
  }
  
  resultFrame$ref.date = sapply(resultFrame$ref.date, parseDate);
  colnames(resultFrame)[1] = "Compahnia";
  colnames(resultFrame)[2] = "Ano";
  resultFrame = do.call(data.frame,lapply(resultFrame, function(x) replace(x, is.infinite(x), 0)))
  View(resultFrame);
  
  saveData(resultFrame, "SIZE");
  
  return (resultFrame);
}

NIVFunc <- function(dataInfo) {
  resultFrame= data.frame();
  isNiv <- function(X) {
    if (X %in% c("Novo Mercado", "Nível 2")) {
      return(1);
    }
    
    return (0)
  }
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    
    try({
      cCode = selectedInfo$company.code;
      cName = selectedInfo$company.name;
      hb =selectedInfo$history.governance.listings[[1]];
      NIV = sapply(hb$listed.segment, isNiv);
      hb["NIV"] = NIV;
      hb["Código"] = cCode;
      resultFrame = rbind(resultFrame, hb[, c(1,2,6, 7)]);  
    });
  }
  
  resultFrame$ref.date = sapply(resultFrame$ref.date, parseDate);
  colnames(resultFrame)[1] = "Compahnia";
  colnames(resultFrame)[2] = "Ano";
  resultFrame = do.call(data.frame,lapply(resultFrame, function(x) replace(x, is.infinite(x), 0)))
  View(resultFrame);
  
  saveData(resultFrame, "NIV");
  
  return (resultFrame);
}

# ROE problema
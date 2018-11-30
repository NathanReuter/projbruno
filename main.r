library("dplyr");
source("utils.R");
#allData = readRDS("./data/allData.rds");
localiza = allData[allData$company.code == "19739",]
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
      QTvalue = round((mergedData$mkt.avg.value + (mergedData$passivoTotal - mergedData$patrimonioLiquido)) / mergedData$ativoTotal, 2);
      mergedData["Código"] = rep(cCode, nrow(mergedData));
      mergedData["Companhia"] = rep(cName, nrow(mergedData));
      mergedData["QT"] = QTvalue;
      mergedData = mergedData[, c(1, 9, 10, 11)];
      resultFrame = rbind(resultFrame, mergedData);  
    })
  }
  
  resultFrame$ref.date = sapply(resultFrame$ref.date, parseDate);
  colnames(resultFrame)[1] = "Ano";
  View(resultFrame);
  
  #saveData(resultFrame, "QT");
  return (resultFrame);
}

#QTFun(allData);

ROEFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb = selectedInfo$fr.cashflow[[1]];
    hb2 = selectedInfo$fr.liabilities[[1]];
    years = sapply(unique(hb$ref.date), parseDate);
    lucroLiquido = hb[hb$acc.number == "6.01.01.01", ];
    patrimonioLiquido = hb2[hb2$acc.number == "2.03", ];
    if (nrow(lucroLiquido) > 0 && nrow(patrimonioLiquido)) {
      ROE = round((lucroLiquido$acc.value / patrimonioLiquido$acc.value), 2);
      partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "ROE"=ROE);
      resultFrame = rbind(resultFrame, partialFrame)
    }
  }
  
  View(resultFrame)
}

#ROEFunc(allData);

PVPAFunc <- function(dataInfo) {
  resultFrame= data.frame();
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb = selectedInfo$history.mkt.value[[1]];
    hb2 = selectedInfo$history.stockholders[[1]]
    hb2 = hb2[hb2$type.register == "Total", ];
    hb3 = selectedInfo$fr.liabilities[[1]]
    hb3 = hb3[hb3$acc.number == "2.03", ];
    years = sapply(unique(hb3$ref.date), parseDate);
    valorDeMercado = hb$mkt.avg.value;
    numeroAcoesEmpresa = as.numeric(hb2$qtd.ord.shares) + as.numeric(hb2$qtd.pref.shares);
    PL = valorDeMercado/numeroAcoesEmpresa;
    patrimonioLiquido = hb3$acc.value * 1000;
    VPA = patrimonioLiquido / numeroAcoesEmpresa;
    PVPA = round(PL/VPA, 2);
    if (length(years) != length(PVPA)) {
      View(years)
    }
    partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "PVPA"=PVPA);
    resultFrame = rbind(resultFrame, partialFrame)
  }
  
  View(resultFrame);
  
  #saveData(resultFrame, "PVPA");
  
  return (resultFrame);
}

#PVPAFunc(allData);

RMDEFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb = selectedInfo$history.compensation[[1]];
    hb = hb[hb$level.remuneration == "Statutory Directors", ];
    years = sapply(unique(hb$ref.date), parseDate);
    RMDE = round(as.numeric(hb$total.value.remuneration)/ as.numeric(hb$qtd.members));
    if (length(RMDE) > 0) {
      if (length(years) != length(RMDE)) {
        hb = hb[complete.cases(hb), ]
        years = sapply(unique(hb$ref.date), parseDate);
        RMDE = round(as.numeric(hb$total.value.remuneration)/ as.numeric(hb$qtd.members), 2);
      }
      partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "RMDE"=RMDE);
      resultFrame = rbind(resultFrame, partialFrame);  
    }
  }
  
  View(resultFrame)
}

#RMDEFunc(allData)

RMCAFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb = selectedInfo$history.compensation[[1]];
    hb = hb[hb$level.remuneration == "Management Council", ];
    years = sapply(unique(hb$ref.date), parseDate);
    RMCA = round(as.numeric(hb$total.value.remuneration)/ as.numeric(hb$qtd.members));
    
    if (length(RMCA) > 0) {
      if (length(years) != length(RMCA)) {
        hb = hb[complete.cases(hb), ]
        years = sapply(unique(hb$ref.date), parseDate);
        RMCA = round(as.numeric(hb$total.value.remuneration)/ as.numeric(hb$qtd.members), 2);
      }
      partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "RMCA"=RMCA);
      resultFrame = rbind(resultFrame, partialFrame);  
    }
  }
  
  View(resultFrame)
}

#RMCAFunc(allData)

# PORCENTAGEM
PRVDEFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb = selectedInfo$history.compensation[[1]];
    hb = hb[hb$level.remuneration == "Statutory Directors", ];
    years = sapply(unique(hb$ref.date), parseDate);
    PRVDE = round((hb$variable.bonus + hb$variable.results.participation 
                   + hb$variable.meetings.participation + hb$ variable.commissions.participation 
                   + hb$ variable.others + hb$stocks.options.benefits) / hb$total.value.remuneration, 4) * 100;
    if (length(PRVDE) > 0) {
      if (length(years) != length(PRVDE)) {
        hb = hb[complete.cases(hb), ]
        years = sapply(unique(hb$ref.date), parseDate);
        PRVDE = round((hb$variable.bonus + hb$variable.results.participation 
                       + hb$variable.meetings.participation + hb$ variable.commissions.participation 
                       + hb$ variable.others + hb$stocks.options.benefits) / hb$total.value.remuneration, 4) * 100;
      }
      partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "PRVDE"=PRVDE);
      resultFrame = rbind(resultFrame, partialFrame);  
    }
  }
  
  View(resultFrame)
}

#PRVDEFunc(allData);


PRVCAFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb = selectedInfo$history.compensation[[1]];
    hb = hb[hb$level.remuneration == "Management Council", ];
    years = sapply(unique(hb$ref.date), parseDate);
    PRVCA = round((hb$variable.bonus + hb$variable.results.participation 
                   + hb$variable.meetings.participation + hb$ variable.commissions.participation 
                   + hb$ variable.others + hb$stocks.options.benefits) / hb$total.value.remuneration, 4) * 100;
    if (length(PRVCA) > 0) {
      if (length(years) != length(PRVCA)) {
        hb = hb[complete.cases(hb), ]
        years = sapply(unique(hb$ref.date), parseDate);
        PRVCA = round((hb$variable.bonus + hb$variable.results.participation 
                       + hb$variable.meetings.participation + hb$ variable.commissions.participation 
                       + hb$ variable.others + hb$stocks.options.benefits) / hb$total.value.remuneration, 4) * 100;
      }
      partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "PRVCA"=PRVCA);
      resultFrame = rbind(resultFrame, partialFrame);  
    }
  }
  
  View(resultFrame)
}

#PRVCAFunc(allData)

# FOI PULADO FALTA A POCPA
# RCEOFunc <- function(dataInfo) {
#   
# }
# PRVCEO PRECISA DA POCPA


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
        filtered["Código"] = cCode;
        filtered["POE"] = POE;
        resultFrame = rbind(resultFrame, filtered[, c(1,2,15, 16)]);  
      }
      
    })
  }
  resultFrame$ref.date = sapply(resultFrame$ref.date, parseDate);
  colnames(resultFrame)[1] = "Companhia";
  colnames(resultFrame)[2] = "Ano";
  resultFrame = do.call(data.frame,lapply(resultFrame, function(x) replace(x, is.infinite(x), 0)))
  View(resultFrame);
  
  #saveData(resultFrame, "POE");
  
  return (resultFrame);
}

removeLastName = function(name) {
  b= strsplit(name, " ");
  return (paste(unlist(b)[1:3], collapse = " "));
}

#POEFunc(allData)
POCPAFunc <- function(dataInfo) {
  resultFrame= data.frame();
  
  for (index in 1:nrow(dataInfo)) {
    selectedInfo = dataInfo[index, ]
    cCode = selectedInfo$company.code;
    cName = selectedInfo$company.name;
    hb =selectedInfo$history.responsible.docs[[1]];
    hb = hb[hb$person.job == "Diretor Presidente",];
    namesResp = sapply(hb$person.name, removeLastName);
    hb$person.name = namesResp;
    hb2 =selectedInfo$history.board.composition[[1]];
    namesConcil = sapply(hb2$person.name, removeLastName);
    hb2$person.name = namesConcil;
    years = sapply(unique(hb$ref.date), parseDate);
    POCPA = vector();
    for (i in 1:nrow(hb)) {
      name = hb[i, ]$person.name;
      year = hb[i, ]$ref.date;
      value = name %in% hb2[hb2$ref.date == year, ]$person.name;
      POCPA = c(POCPA, as.integer(value));
    }
    
    if (length(years)*length(POCPA) > 0  ) {
      if (length(years) != length(POCPA)) {
        hb = hb[complete.cases(hb), ];
        years = sapply(unique(hb$ref.date), parseDate);
        POCPA = vector();
        for (i in 1:nrow(hb)) {
          name = hb[i, ]$person.name;
          year = hb[i, ]$ref.date;
          value = name %in% hb2[hb2$ref.date == year, ]$person.name;
          POCPA = c(POCPA, as.integer(value));
        }
      }
      try({
        partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "POCPA"=POCPA);
        resultFrame = rbind(resultFrame, partialFrame);  
      })
    }
  }
  
  View(resultFrame)
  
  return (resultFrame);
}

#POCPA = POCPAFunc(allData)


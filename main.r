library("dplyr");
source("utils.R");
#allData = readRDS("./data/allData.rds");
localiza = allData[allData$company.code == "19739",]
#Indicador do Preço/Lucro (PL)

builData = function () {
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
  PL = PLFunc(allData);
  
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
  
  QT = QTFun(allData);
  
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
    return(resultFrame);
  }
  
  ROE = ROEFunc(allData);
  
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
  
  PVPA = PVPAFunc(allData);
  
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
    return(resultFrame);
  }
  
  RMDE = RMDEFunc(allData)
  
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
    return(resultFrame);
  }
  
  RMCA = RMCAFunc(allData)
  
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
    
    View(resultFrame);
    return(resultFrame);
  }
  
  PRVDE = PRVDEFunc(allData);
  
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
    return(resultFrame);
  }
  
  PRVCA = PRVCAFunc(allData)
  
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
  
  POE= POEFunc(allData)
  
  removeLastName = function(name) {
    b= strsplit(name, " ");
    return (paste(unlist(b)[1:3], collapse = " "));
  }
  
  POCPEFunc <- function(dataInfo) {
    resultFrame= data.frame();
    
    for (index in 1:nrow(dataInfo)) {
      selectedInfo = dataInfo[index, ]
      cCode = selectedInfo$company.code;
      cName = selectedInfo$company.name;
      hb =selectedInfo$history.board.composition[[1]];
      years = sapply(unique(hb$ref.date), parseDate);
      POCPE = vector();
      for (i in 1:length(unique(hb$ref.date))) {
        filtered = hb[hb$ref.date == unique(hb$ref.date)[i], ];
        filtered = filtered[filtered$code.type.job == 30, ]
        name = filtered[i, ]$person.name;
        year = filtered[i, ]$ref.date;
        if (nrow(filtered) > 0) {
          value = TRUE
        } else {
          value = FALSE;
        }
        POCPE = c(POCPE, as.integer(value));
      }
      
      if (length(years)*length(POCPE) > 0  ) {
        
        try({
          if (length(years) != length(POCPE)) {
            hb = hb[complete.cases(hb), ];
            years = sapply(unique(hb$ref.date), parseDate);
            POCPE = vector();
            for (i in 1:length(unique(hb$ref.date))) {
              filtered = hb[hb$ref.date == unique(hb$ref.date)[i], ];
              filtered = filtered[filtered$code.type.job == 30, ]
              name = filtered[i, ]$person.name;
              year = filtered[i, ]$ref.date;
              if (nrow(filtered) > 0) {
                value = TRUE
              } else {
                value = FALSE;
              }
              POCPE = c(POCPE, as.integer(value));
            }
          }
          partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "POCPE"=POCPE);
          resultFrame = rbind(resultFrame, partialFrame);  
        })
      }
    }
    
    View(resultFrame)
    
    return (resultFrame);
  }
  
  POCPE = POCPEFunc(allData);
  
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
  
  POCPA = POCPAFunc(allData)
  
  RCEOFunc <- function(dataInfo, POCPA) {
    resultFrame= data.frame();
    
    for (index in 1:nrow(dataInfo)) {
      selectedInfo = dataInfo[index, ]
      cCode = selectedInfo$company.code;
      RCEO = vector();
      try({
        cName = selectedInfo$company.name;
        hb =selectedInfo$history.compensation.summary[[1]];
        values = POCPA[POCPA$Código == cCode , ]
        years = sort(unique(POCPA$Ano), decreasing = TRUE);
        years = years[2: length(years)];
        for (year in years) {
          hbYears = sapply(hb$ref.date, parseDate);
          filtered = hb[hbYears == year, ];
          value = values[values$Ano == year, ]$POCPA
          if (value == 1) {
            filtered = filtered[filtered$level.remuneration == "Management Council", ];
          } else {
            filtered = filtered[filtered$level.remuneration == "Statutory Directors", ];
          }
          
          RCEO = c(RCEO, filtered$max.remuneration[1]);
        } 
        
        if (length(RCEO) > 0) {
          partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "RCEO"=RCEO);
          resultFrame = rbind(resultFrame, partialFrame);  
        }
        
      })
    }
    View(resultFrame);
    
    #saveData(resultFrame, "POE");
    
    return (resultFrame);
  }
  
  RCEO = RCEOFunc(allData, POCPA)
  
  PRVCEOFunc <- function(dataInfo, POCPA) {
    resultFrame= data.frame();
    setValue = function(i, val1, val2, ceoIsMember) {
      print(i)
      if (ceoIsMember[i]) {
        return (val1[i]);
      }
      
      return (val2[i]);
    }
    
    for (index in 1:nrow(dataInfo)) {
      selectedInfo = dataInfo[index, ]
      cCode = selectedInfo$company.code;
      cName = selectedInfo$company.name;
      hb = selectedInfo$history.compensation[[1]];
      hb2 = selectedInfo$history.compensation[[1]];
      filtered= POCPA[POCPA$POCPA == 1, ] ;
      filtered = filtered[filtered$Código == cCode, ] 
      hb = hb[hb$level.remuneration == "Management Council", ];
      value1 = round((hb$variable.bonus + hb$variable.results.participation 
                      + hb$variable.meetings.participation + hb$ variable.commissions.participation 
                      + hb$ variable.others + hb$stocks.options.benefits) / hb$total.value.remuneration, 4) * 100;
      hb2 = hb2[hb$level.remuneration == "Statutory Directors", ];
      years = sapply(unique(hb2$ref.date), parseDate);
      value0 = round((hb2$variable.bonus + hb2$variable.results.participation 
                      + hb2$variable.meetings.participation + hb2$ variable.commissions.participation 
                      + hb2$ variable.others + hb2$stocks.options.benefits) / hb2$total.value.remuneration, 4) * 100;
      PRVCEO = sapply(seq_along(filtered$POCPA), setValue, val1 = value1, val2 = value0, ceoIsMember = filtered$POCPA)
      if (length(PRVCEO) > 0) {
        if (length(years) != length(PRVCEO)) {
          hb = selectedInfo$history.compensation[[1]];
          hb2 = selectedInfo$history.compensation[[1]];
          filtered= POCPA[POCPA$POCPA == 1, ] ;
          filtered = filtered[filtered$Código == cCode, ] 
          hb = hb[hb$level.remuneration == "Management Council", ];
          years = sapply(unique(hb$ref.date), parseDate);
          value1 = round((hb$variable.bonus + hb$variable.results.participation 
                          + hb$variable.meetings.participation + hb$ variable.commissions.participation 
                          + hb$ variable.others + hb$stocks.options.benefits) / hb$total.value.remuneration, 4) * 100;
          hb2 = hb2[hb$level.remuneration == "Statutory Directors", ];
          years = sapply(unique(hb2$ref.date), parseDate);
          value0 = round((hb2$variable.bonus + hb2$variable.results.participation 
                          + hb2$variable.meetings.participation + hb2$ variable.commissions.participation 
                          + hb2$ variable.others + hb2$stocks.options.benefits) / hb2$total.value.remuneration, 4) * 100;
          PRVCEO = sapply(seq_along(filtered$POCPA), setValue, val1 = value1, val2 = value0, ceoIsMember = filtered$POCPA)
        }
        colnames(filtered)[4] = "PRVCEO";
        filtered$PRVCEO = PRVCEO;
        resultFrame = rbind(resultFrame, filtered);  
      }
    }
    
    View(resultFrame)
    return(resultFrame);
  }
  
  PRVCEO = PRVCEOFunc(allData, POCPA);
  
  AM = loadData("AM");
  
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
        parcialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=parseDate(filtered$ref.date[1]), "PC"=value);
        resultFrame = rbind(resultFrame, parcialFrame);
      }
    }
    
    View(resultFrame);
    
    #saveData(resultFrame, "PC");
    
    return (resultFrame);
  }
  
  PC = PCFunc(allData)
  
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
    colnames(resultFrame)[1] = "Companhia";
    colnames(resultFrame)[2] = "Ano";
    resultFrame = do.call(data.frame,lapply(resultFrame, function(x) replace(x, is.infinite(x), 0)))
    View(resultFrame);
    
    saveData(resultFrame, "NIV");
    
    return (resultFrame);
  };
  
  NIV = NIVFunc(allData)
  
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
    colnames(resultFrame)[1] = "Companhia";
    colnames(resultFrame)[2] = "Ano";
    resultFrame = do.call(data.frame,lapply(resultFrame, function(x) replace(x, is.infinite(x), 0)))
    View(resultFrame);
    
    saveData(resultFrame, "SIZE");
    
    return (resultFrame);
  }
  
  SIZE = SIZEFunc(allData)
  
  APCFunc <- function(dataInfo) {
    resultFrame= data.frame();
    
    for (index in 1:nrow(dataInfo)) {
      selectedInfo = dataInfo[index, ]
      cCode = selectedInfo$company.code;
      cName = selectedInfo$company.name;
      hb = selectedInfo$history.stockholders[[1]];
      hb = hb[hb$type.stockholder == "Fisica",];
      hb2 = selectedInfo$history.board.composition[[1]];
      years = sapply(unique(hb$ref.date), parseDate);
      APC = vector()
      for (i in 1:length(unique(hb$ref.date))) {
        year = unique(hb$ref.date)[i]; 
        filtered = hb[hb$ref.date == year ,];
        filtered2 = hb2[hb2$ref.date == year,];
        filtered = filtered[filtered$name.stockholder %in% filtered2$person.name,]
        value = sum(as.numeric(filtered$perc.ord.shares) + as.numeric(filtered$perc.pref.shares));
        APC = c(APC, value);
      }
      try({
        if (length(APC) > 0) {
          partialFrame = data.frame("Companhia"=cName, "Código"=cCode, "Ano"=years, "APC"=APC);
          resultFrame = rbind(resultFrame, partialFrame)
        }
      })
    }
    
    View(resultFrame)
    return(resultFrame);
  }
  
  APC = APCFunc(allData)
}


generateRankFunc <- function(DATA) {
  name = deparse(substitute(DATA));
  sorted = DATA[order(-DATA[name]),];
  years = unique(DATA$Ano);
  resultFrame = data.frame();
  for (year in years) {
    filtered = filter(sorted, Ano == year);
    nElements = nrow(filtered);
    Rank =  nElements - as.numeric(rownames(filtered));
    RQTit = (Rank)/(nElements - 1)
    filtered[paste("R",name, sep = "")] = RQTit;
    resultFrame = rbind(resultFrame, filtered);
  }
  
  resultFrame = resultFrame[, -which(names(resultFrame) == name)];
  #View(resultFrame);
  #saveData(resultFrame, "RPRV");
  
  return (resultFrame);
}

generateRankFunc(APC)

completaFunc = function(PL, QT, ROE, PVPA, RMDE, RMCA, PRVDE, PRVCA, RCEO, PRVCEO, POE, POCPE, POCPA, AM, APC, PC,SIZE, NIV) {
  merged = merge(PL, QT, all = TRUE);
  merged = merge(merged, ROE, all = TRUE);
  merged = merge(merged, PVPA, all = TRUE);
  merged = merge(merged, RMDE, all = TRUE);
  merged = merge(merged, RMCA, all = TRUE);
  merged = merge(merged, PRVDE, all = TRUE);
  merged = merge(merged, PRVCA, all = TRUE);
  merged = merge(merged, RCEO, all = TRUE);
  merged = merge(merged, PRVCEO, all = TRUE);
  merged = merge(merged, POE, all = TRUE);
  merged = merge(merged, POCPE, all = TRUE);
  merged = merge(merged, POCPA, all = TRUE);
  merged = merge(merged, AM, all = TRUE);
  merged = merge(merged, APC, all = TRUE);
  merged = merge(merged, PC, all = TRUE);
  merged = merge(merged, SIZE, all = TRUE);
  merged = merge(merged, NIV, all = TRUE);
  View(merged);
  return(merged)
}

completaRankFunc = function(PL, QT, ROE, PVPA, RMDE, RMCA, PRVDE, PRVCA, RCEO, PRVCEO, POE, POCPE, POCPA, AM, APC, PC,SIZE, NIV) {
  merged = merge(generateRankFunc(PL), generateRankFunc(QT), all = TRUE);
  merged = merge(merged, generateRankFunc(ROE), all = TRUE);
  merged = merge(merged, generateRankFunc(PVPA), all = TRUE);
  merged = merge(merged, generateRankFunc(RMDE), all = TRUE);
  merged = merge(merged, generateRankFunc(RMCA), all = TRUE);
  merged = merge(merged, generateRankFunc(PRVDE), all = TRUE);
  merged = merge(merged, generateRankFunc(PRVCA), all = TRUE);
  merged = merge(merged, generateRankFunc(RCEO), all = TRUE);
  merged = merge(merged, generateRankFunc(PRVCEO), all = TRUE);
  merged = merge(merged, generateRankFunc(POE), all = TRUE);
  merged = merge(merged, generateRankFunc(POCPE), all = TRUE);
  merged = merge(merged, generateRankFunc(POCPA), all = TRUE);
  merged = merge(merged, generateRankFunc(AM), all = TRUE);
  merged = merge(merged, generateRankFunc(APC), all = TRUE);
  merged = merge(merged, generateRankFunc(PC), all = TRUE);
  merged = merge(merged, generateRankFunc(SIZE), all = TRUE);
  View(merged);
  
  
  return(merged)
}

completaRank = completaRankFunc(PL, QT, ROE, PVPA, RMDE, RMCA, PRVDE, PRVCA, RCEO, PRVCEO, POE, POCPE, POCPA, AM, APC, PC, SIZE, NIV);
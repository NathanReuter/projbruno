# Exemple to fetch information from the info dataframe and rebuild into a dataframe again

getCompanyCode <- function(name, haslevels) {
  if (is.na(name)) {
    return (0);
  }
  
  if (missing(haslevels)) {
    haslevels = FALSE;
  }
  if (haslevels && typeof(name) != "character") {
    name = str(droplevels(name));
  }

  #print(name)
  code = (filter(codeAndName, grepl(name, company.name, ignore.case=TRUE)));
  if (length(code) == 0L || nrow(code) < 1) {
    return (0);
  }
  
  return (code[[1,1]]);
}

# P1 - Número de processos judiciais sofridos pela empresa
# Pegar da tabela processos.xl, e padronizar por ano

p1 <- function (dataInfo, filterNull) {
  if (missing(filterNull)) {
    filterNull = TRUE;
  }
  # todo, method to get all companies code
  codeVector = vector();
  companyVector = vector();
  processVector = vector();
  yearVector = vector();
  yearCounter <- 0;
  
  for (yearInfo in dataInfo) {
    yearCounter <- yearCounter + 1;
    j = length(companyVector);
    year = names(dataInfo)[yearCounter];
    
    for (i in 1:length(yearInfo[[1]])) {
      company = yearInfo[[i, 1]];
      codeVector[j + i] = getCompanyCode(company, FALSE)[1];
      companyVector[j + i] <- company;
      processVector[j + i] <- yearInfo[[i, 2]];
      yearVector[j + i] <- year;
    }
  }
  
  resultFrame = data.frame(
    "Código" = codeVector, 
    "Companhia" = companyVector, "Número Processos" = processVector, "Ano" = yearVector);
  if (filterNull) {
    resultFrame = filter(resultFrame, !Código == 0);  
  }
  
  View(resultFrame)
  
  return (resultFrame);
}

# Planilha 2 - Porcentagem em renda variável na remuneração dos executivos 
# AKA variableIncomePercentage
p2 <- function (dataInfo) {
  # get all variables. in history.compensation and divide to total.value.remunaration
  # And divid it in 3 columns Management Council, Statutory Directors, Fiscal Council
  # COLOCAR Data como apenas o Ano
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  MCVector = vector();
  SDVector = vector();
  FCVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hComp = company['history.compensation'];
    hComp = hComp$history.compensation[[1]];
    
    
    if (length(hComp) > 0) {
      localYearVector  = vector();
      localCompany = vector();
      localCode = vector();
      localMC = vector();
      localSD = vector();
      localFC = vector();
      cName = hComp[[1, 1]];
      calculateValue <- function(index) {
        varibleSum = hComp$variable.bonus[index] + hComp$variable.results.participation[index] 
        + hComp$variable.meetings.participation[index] + hComp$variable.others[index] + hComp$variable.results.participation[index];
        result = (varibleSum / hComp$total.value.remuneration[index]) * 100;
        
        if (is.nan(result)) {
          return (0)
        }
        
        return (round(result, 2));
      }
      
      for (index in seq_along(hComp$ref.date)) {
        parsedYear = parseDate(hComp$ref.date[index]);
        
        if (!(parsedYear %in% localYearVector)) {
          localYearVector <- c(localYearVector, parsedYear);
          localCompany <- c(localCompany, cName);
          localCode <- c(localCode, getCompanyCode(cName));
        }
        
        type = hComp$level.remuneration[index];
        value = calculateValue(index);
        switch (type,
                "Management Council" = localMC <- c(localMC, value),
                "Statutory Directors" = localSD <- c(localSD, value),
                "Fiscal Council" = localFC <- c(localFC, value)
        );
      }
      
      resultList = normalizedRowNumbers(list(localMC, localSD, localFC), 0);
      localMC <- resultList[[1]];
      localSD <- resultList[[2]];
      localFC <- resultList[[3]];
      yearVector <<- c(yearVector, localYearVector);
      CompanyVector <<- c(CompanyVector, localCompany);
      CodeVector <<- c(CodeVector, localCode);
      MCVector <<- c(MCVector, localMC);
      SDVector <<- c(SDVector, localSD);
      FCVector <<- c(FCVector, localFC);
    }
  });
  
  resultList = normalizedRowNumbers(list(MCVector, SDVector, FCVector), length(CodeVector));
  
  resultFrame = data.frame(
    "Codigo" = CodeVector, 
    "Companhia" = CompanyVector, 
    "Ano" = yearVector,
    "Management Council" = resultList[[1]], 
    "Statutory Directors" = resultList[[2]], 
    "Fiscal Council" = resultList[[3]]
  );
  
  return(resultFrame);
}

# P3 - Porcentagem de conselheiros independentes
p3 <- function(dataInfo) {
  #TODO CHECK FOR ODER PROBLEM IN YEAR
  # Get code.type  == 27 in history.board.composition and divide by all ocurrences in the year
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  MandateVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hBoardComposition = company['history.board.composition'];
    hBoardComposition = hBoardComposition$history.board.composition[[1]];
    
    if (!is.null(hBoardComposition)) {
      hBoardComposition = filter(hBoardComposition, code.type.job == 27)
      
      if (nrow(hBoardComposition) > 0) {
        localYearVector  = vector();
        localCompany = vector();
        localCode = vector();
        cName = hBoardComposition[[1, 1]];
        localMandatesVector = vector();
        auxmandatesVector = vector();
        for (index in seq_along(hBoardComposition$ref.date)) {
          parsedYear = parseDate(hBoardComposition$ref.date[index]);
          
          if (!(parsedYear %in% localYearVector)) {
            localYearVector <- c(localYearVector, parsedYear);
            localCompany <- c(localCompany, cName);
            localCode <- c(localCode, getCompanyCode(cName));
            localMandatesVector <- c(localMandatesVector, mean(auxmandatesVector));
            auxmandatesVector <- vector();
          }
          mandates = hBoardComposition$qtd.consecutive.mandates[index];
          if (!is.na(mandates)) {
            auxmandatesVector <- c(auxmandatesVector, mandates)
          }
        }
        
        yearVector <<- c(yearVector, localYearVector);
        CompanyVector <<- c(CompanyVector, localCompany);
        CodeVector <<- c(CodeVector, localCode);
        MandateVector <<- c(MandateVector, localMandatesVector);
      }
    }
    
  });
  
  resultFrame = data.frame(
    "Codigo" = CodeVector, 
    "Companhia" = CompanyVector, 
    "Ano" = yearVector,
    "Porcentagem Conselheiros" = MandateVector
  );
  
  return (resultFrame);
}

#p4
# Usar a 7 para saber quem são os CEO e verficar as pessoas fiísicas em history.stockholders, pegar todos menos o CEO.
# Depois pegar todas s pessoas fisicas e comparar quem faz parte do conselho em history.board.composition
# e somar as ações dessas pessoas

p4 <- function(dataInfo, plan7) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  OrderShareVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hStockHolders = company$history.stockholders[[1]];
    if (!is.null(hStockHolders)) {
      hStockHolders = filter(hStockHolders, type.stockholder == "Fisica");
      ceoInConsulList = filter(plan7, Companhia == hStockHolders$name.company[1]);
      
      if (nrow(ceoInConsulList) > 0 && nrow(hStockHolders) > 0) {
        hStockHolders = filter(hStockHolders, !hStockHolders$name.stockholder %in% ceoInConsulList$Diretor);
      }
      
      if (nrow(hStockHolders) > 0) {
        localYearVector  = vector();
        localCompany = vector();
        localCode = vector();
        localDirector = vector();
        localOrderShare = vector();
        cName = hStockHolders[[1, 1]];
        oldYear = 0;
        sumVector = vector();
        # TODO REMOVE CEO
        for (index in 1:nrow(hStockHolders)) {
          value = as.numeric(hStockHolders$perc.pref.shares[index]) + as.numeric(hStockHolders$perc.ord.shares[index])
          sumVector <- c(sumVector, round(value, 2));
          actualYear = parseDate(hStockHolders$ref.date[index])
          
          if (oldYear != actualYear) {
            localOrderShare <- c(localOrderShare, sum(sumVector));
            sumVector <- vector();
            localYearVector <- c(localYearVector, actualYear);
            localCompany <- c(localCompany, cName);
            localCode <- c(localCode, getCompanyCode(cName));
          }
          oldYear = actualYear;
        }
        
        yearVector <<- c(yearVector, localYearVector);
        CompanyVector <<- c(CompanyVector, localCompany);
        CodeVector <<- c(CodeVector, localCode);
        OrderShareVector <<- c(OrderShareVector, localOrderShare);
      }
    }
    
    
  });
  
  resultFrame = data.frame(
    "Codigo" = CodeVector,
    "Companhia" = CompanyVector,
    "Ano" = yearVector,
    "APC" = OrderShareVector
  );
  
  return(resultFrame);
}
# p5 Remuneração Média (RM)
# pegar no history.compensation e dividir total.value / qtd.members
p5 <- function(dataInfo) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  AverageRemunaration = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hComp = company['history.compensation'];
    hComp = hComp$history.compensation[[1]];
    
    
    if (length(hComp) > 0) {
      localYearVector  = vector();
      localCompany = vector();
      localCode = vector();
      localAR = vector();
      localAuxAR = vector();
      cName = hComp[[1, 1]];
      
      
      for (index in seq_along(hComp$ref.date)) {
        parsedYear = parseDate(hComp$ref.date[index]);
        if (!(parsedYear %in% localYearVector)) {
          if (index != 1) {
            localAR <- c(localAR, sum(localAuxAR));
            localAuxAR <- vector();  
          }
          localYearVector <- c(localYearVector, parsedYear);
          localCompany <- c(localCompany, cName);
          localCode <- c(localCode, getCompanyCode(cName));
          
        }
        average = hComp$total.value.remuneration[index]/ hComp$qtd.members[index];
        if (is.nan(average) || is.infinite(average)) {
          average = 0;
        }
        localAuxAR <- c(localAuxAR, average);
      }
      
      localAR <- c(localAR, sum(localAuxAR));
      yearVector <<- c(yearVector, localYearVector);
      CompanyVector <<- c(CompanyVector, localCompany);
      CodeVector <<- c(CodeVector, localCode);
      AverageRemunaration <<- c(AverageRemunaration, localAR);
    }
  });
  
  resultFrame = data.frame(
    "Codigo" = CodeVector,
    "Companhia" = CompanyVector,
    "Ano" = yearVector,
    "Remuneração Média" = AverageRemunaration
  );

  return(resultFrame);
}

# p6 Ranking da Remuneração Média (RRM)
# Fazer Global, para todas as empresas e rankear entre 0-1 o as empresas de acrodo com a remuneração média
p6 <- function(plan5) {
  sortedPlan = plan5[order(-plan5$Remuneração.Média),];
  index = 1;
  years = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017);
  RMRankitVector = vector();
  RRMVector = vector();
  
  for (year in years) {
    inYearComp = filter(sortedPlan, sortedPlan$Ano == year);
    total = nrow(inYearComp);
    for (index in 1:total) {
      RMRankit = total - index + 1;
      RMRankitVector <- c(RMRankitVector, RMRankit);
      RMM = round((RMRankit - 1)/(total -1), 2);
      RRMVector <- c(RRMVector, RMM);
    }
  }
  View(RMRankitVector);
  sortedPlan["RMRankit"] = RMRankitVector;
  sortedPlan["RMM"] = RRMVector;
  resultFrame = sortedPlan[order(-sortedPlan$Remuneração.Média),];
  
  
  return (resultFrame);
}

# p7 Nível percentual de ações em posse dos executivos
# Vasculhar history responsable e pegar os person.name com person.job == "Diretor Presidente"
# Depois usar o person.name e em history.stockholders, verificar se person.name == name.stockholder
# e captura o perc.ord.shares
p7 <- function(dataInfo) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  DirectorVector = vector();
  OrderShareVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hResp = company['history.responsible.docs']$history.responsible.docs[[1]];
    if (!is.null(hResp)) {
      hResp = filter(hResp, person.job %in% c("Diretor Presidente", "Diretor Presidente/Relações com Investidores"));
      hStockHolders = company$history.stockholders[[1]];
      
      if (nrow(hResp) > 0 && nrow(hStockHolders) > 0 ) {
        localYearVector  = vector();
        localCompany = vector();
        localCode = vector();
        localDirector = vector();
        localOrderShare = vector();
        cName = hResp[[1, 1]];
        
        for (name in hResp$person.name) {
          result = filter(hStockHolders, name.stockholder == name);
          #TODO CHECK FOR MORE DE UM RESULT AND CHECK FOR NULL BEFORE FILTER
          if (nrow(result) > 0) {
            for (index in 1:nrow(result)) {
              localDirector <- c(localDirector, name);
              localYearVector <- c(localYearVector, parseDate(result$ref.date[index]));
              localCompany <- c(localCompany, cName);
              localOrderShare <- c(localOrderShare, result$perc.ord.shares[index]);
              localCode <- c(localCode, getCompanyCode(cName));  
            }
          }
        }
        
        yearVector <<- c(yearVector, localYearVector);
        CompanyVector <<- c(CompanyVector, localCompany);
        CodeVector <<- c(CodeVector, localCode);
        DirectorVector <<- c(DirectorVector, localDirector);
        OrderShareVector <<- c(OrderShareVector, localOrderShare);
      }
    }
    
    
  });
  
  resultFrame = data.frame(
    "Codigo" = CodeVector,
    "Companhia" = CompanyVector,
    "Ano" = yearVector,
    "Diretor" = DirectorVector,
    "Percentual de Ações" = OrderShareVector
  );
  
  return(resultFrame);
}
# p8 Dummy 1 - CEO presidente do conselho; 0 - Caso contrário
# Varrer history.board.composition, e para cada ano se tiver cõdigo 30 dummy = 1 se nao 0
p8 <- function(dataInfo) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  DummyVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hBC = company$history.board.composition[[1]];
    
    if (!is.null(hBC) && nrow(hBC) > 0) {
      localYearVector  = vector();
      localCompany = vector();
      localCode = vector();
      localDummyVector = vector();
      cName = hBC[[1, 1]];
      
      for (index in seq_along(hBC$ref.date)) {
        parsedYear = parseDate(hBC$ref.date[index])
        
        if (!(parsedYear %in% localYearVector)) {
          localYearVector <- c(localYearVector, parsedYear);
          localCompany <- c(localCompany, cName);
          localCode <- c(localCode, getCompanyCode(cName));
          result = filter(hBC, ref.date == ref.date[index],code.type.job == 30);
          if (nrow(result) > 0) {
            localDummyVector <- c(localDummyVector, 1);
          } else {
            localDummyVector <- c(localDummyVector, 0);
          }
        }
      }
      
      yearVector <<- c(yearVector, localYearVector);
      CompanyVector <<- c(CompanyVector, localCompany);
      CodeVector <<- c(CodeVector, localCode);
      DummyVector <<- c(DummyVector, localDummyVector);
    }
  });
  
  resultFrame = data.frame(
    "Código" = CodeVector,
    "Companhia" = CompanyVector,
    "Ano" = yearVector,
    "POC" = DummyVector
  );
  return(resultFrame);
}
# p9 Tempo de mandato em anos do CEO na empresa
# Olhar history.responsible , verificar todos os person.job = "Diretor Presidente"
# Varrer os anos de 2010 e contar quantos anos de mandato 
# Ano         Diretor:             Tempo:
# 2012        ----                  1
# 2011        Marcos Antonio Molina dos Santos    2
# 2010        Marcos Antonio Molina dos Santos    2
p9 <- function(dataInfo) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  DirectorVector = vector();
  MandateTimeVector = vector();
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hResp = company['history.responsible.docs']$history.responsible.docs[[1]];
    if (!is.null(hResp)) {
      hResp = filter(hResp, person.job %in% c("Diretor Presidente", "Diretor Presidente/Relações com Investidores"));
      
      if (nrow(hResp) > 0) {
        localYearVector  = vector();
        localCompany = vector();
        localCode = vector();
        localDirector = vector();
        localMandateTime = vector();
        cName = hResp[[1, 1]];
        actualDirectorName = "";
        yearsCounter = 0;
        for (index in 1:nrow(hResp)) {
          if (!is.null(hResp$person.name[index]) && !is.na(hResp$person.name[index])) {
            if (actualDirectorName == "" || actualDirectorName == hResp$person.name[index]) {
              yearsCounter = yearsCounter + 1;
            } else {
              localMandateTime <- c(localMandateTime, rep(c(yearsCounter), yearsCounter));
              yearsCounter = 1;
            }
            
            actualDirectorName = hResp$person.name[index];
            localDirector <- c(localDirector, hResp$person.name[index]);
            localYearVector <- c(localYearVector, parseDate(hResp$ref.date[index]));
            localCompany <- c(localCompany, cName);
            localMandateTime <- c(localMandateTime, hResp$perc.ord.shares[index]);
            localCode <- c(localCode, getCompanyCode(cName));  
          }
        }
        localMandateTime <- c(localMandateTime, rep(c(yearsCounter), yearsCounter));
        yearVector <<- c(yearVector, localYearVector);
        CompanyVector <<- c(CompanyVector, localCompany);
        CodeVector <<- c(CodeVector, localCode);
        DirectorVector <<- c(DirectorVector, localDirector);
        MandateTimeVector <<- c(MandateTimeVector, localMandateTime);
      }
    }
    
    
  });
  
  resultFrame = data.frame(
    "Codigo" = CodeVector,
    "Companhia" = CompanyVector,
    "Ano" = yearVector,
    "Diretor" = DirectorVector,
    "Tempo de Mandato (Anos)" = MandateTimeVector
  );
  
  
  return(resultFrame);
}

# p10 Logaritmo natural do total de ativos de uma empresa
# Pegar a planilha Bruno.xls
# Color no formato codigo, nome, e pegar o valor de total de ativos e aplica Ln em todos os anos(COlunas)
p10 <- function(dataInfo, filterNull) {
  if (missing(filterNull)) {
    filterNull = TRUE;
  }
  codeVector = vector();
  companyVector = vector();
  activeValueVector = vector();
  yearVector = vector();
  yearCounter <- 2009;
  j <- 0;
  for (colSheet in  brunoSheet[5: length(brunoSheet)]) {
    j = j + length(companyVector)
    yearCounter = yearCounter + 1;
    i = 0;
    k = 3
    for (activeValue in colSheet[k + 1: length(colSheet)]) {
      i = i + 1;
      cName = unlist(brunoSheet[k + i, 2]);
      aValue = as.numeric(unlist(activeValue));
      codeVector[i + j] = getCompanyCode(cName, TRUE);
      companyVector[i + j] = cName;
      yearVector[i + j] = yearCounter;
      activeValueVector[i + j] = log(aValue);
    }
  }
  
  resultFrame = data.frame("Código" = codeVector, "Companhia" = companyVector, "Ano" = yearVector, 'Valor Ativo' = activeValueVector);
  if (filterNull) {
    resultFrame = filter(resultFrame, !Código == 0);  
  }
  View(resultFrame)
  
  return (resultFrame)
}

# p11 Dummy 1 - Novo Mercado ou Nível 2; 0 - Caso contrário
# history.gorvernance.listings
# listed.segment = "Novo Mercado" || "Nivel 2"
# Lista por ano e empresa
p11 <- function(dataInfo) {
  yearVector = vector();
  CompanyVector = vector();
  CodeVector = vector();
  DummyVector = vector();
  searchTarget = c("Novo Mercado", "Nivel 2");
  
  by(dataInfo, 1:nrow(dataInfo), function(company) {
    hGL = company$history.governance.listings[[1]];
    
    if (!is.null(hGL) && nrow(hGL) > 0) {
      localYearVector  = vector();
      localCompany = vector();
      localCode = vector();
      localDummyVector = vector();
      cName = hGL[[1, 1]];
      
      for (index in seq_along(hGL$ref.date)) {
        parsedYear = parseDate(hGL$ref.date[index])
        
        if (!(parsedYear %in% localYearVector)) {
          localYearVector <- c(localYearVector, parsedYear);
          localCompany <- c(localCompany, cName);
          localCode <- c(localCode, getCompanyCode(cName));
          result = filter(hGL, ref.date == ref.date[index],listed.segment %in% searchTarget);
          if (nrow(result) > 0) {
            localDummyVector <- c(localDummyVector, 1);
          } else {
            localDummyVector <- c(localDummyVector, 0);
          }
        }
      }
      
      yearVector <<- c(yearVector, localYearVector);
      CompanyVector <<- c(CompanyVector, localCompany);
      CodeVector <<- c(CodeVector, localCode);
      DummyVector <<- c(DummyVector, localDummyVector);
    }
  });
  
  resultFrame = data.frame(
    "Código" = CodeVector,
    "Companhia" = CompanyVector,
    "Ano" = yearVector,
    "Dummy" = DummyVector
  );
  
  
  
  return(resultFrame);
}


# p12 Dummy 1 - Setor que será observado; 0 - Caso contrário 
# Separa pela planilha bruno.xls código, nomeCOmpania e nomesetor
p12 <- function(dataInfo, filterNull) {
  if (missing(filterNull)) {
    filterNull = TRUE;
  }
  codeVector = vector();
  companyVector = vector();
  activeValueVector = vector();
  yearVector = vector();
  yearCounter <- 2009;
  j <- 0;
  for (colSheet in  brunoSheet[5: length(brunoSheet)]) {
    j = j + length(companyVector)
    yearCounter = yearCounter + 1;
    i = 0;
    k = 3
    for (activeValue in colSheet[k + 1: length(colSheet)]) {
      i = i + 1;
      cName = unlist(brunoSheet[k + i, 2]);
      aValue = as.numeric(unlist(activeValue));
      codeVector[i + j] = getCompanyCode(cName, TRUE);
      companyVector[i + j] = cName;
      yearVector[i + j] = yearCounter;
      activeValueVector[i + j] = unlist(brunoSheet[k + i, 4]);
    }
  }
  
  resultFrame = data.frame("Código" = codeVector, "Companhia" = companyVector, "Ano" = yearVector, 'Valor Ativo' = activeValueVector);
  if (filterNull) {
    resultFrame = filter(resultFrame, !Código == 0);  
  }
  View(resultFrame)
  return (resultFrame)
}

writePlanilhas <- function () {
  write.xlsx2(planilha1, "./excels/Planilha1.xlsx");
  write.xlsx2(planilha2, "./excels/Planilha2.xlsx");
  write.xlsx2(planilha3, "./excels/Planilha3.xlsx");
  write.xlsx2(planilha4, "./excels/Planilha4.xlsx");
  write.xlsx2(planilha5, "./excels/Planilha5.xlsx");
  write.xlsx2(planilha6, "./excels/Planilha6.xlsx");
  write.xlsx2(planilha7, "./excels/Planilha7.xlsx");
  write.xlsx2(planilha8, "./excels/Planilha8.xlsx");
  write.xlsx2(planilha9, "./excels/Planilha9.xlsx");
  write.xlsx2(planilha10, "./excels/Planilha10.xlsx");
  write.xlsx2(planilha11, "./excels/Planilha11.xlsx");
  write.xlsx2(planilha12, "./excels/Planilha12.xlsx");
}

readPlanilhas <- function() {
  planilha1 = read.csv("./completas/Planilha 1.csv");
  planilha2 = read.csv("./completas/Planilha 2.csv");
  planilha3 = read.csv("./completas/Planilha 3.csv");
  planilha4 = read.csv("./completas/Planilha 4.csv");
  planilha5 = read.csv("./completas/Planilha 5.csv");
  planilha6 = read.csv("./completas/Planilha 6.csv");
  planilha7 = read.csv("./completas/Planilha 7.csv");
  planilha8 = read.csv("./completas/Planilha 8.csv");
  planilha9 = read.csv("./completas/Planilha 9.csv");
  planilha10 = read.csv("./completas/Planilha 10.csv");
  planilha11 = read.csv("./completas/Planilha 11.csv");
  planilha12 = read.csv("./completas/Planilha 12.csv");  
}

fullSheet = function() {
  PJVector = vector();
  MCVector = vector();
  SDVector = vector();
  FCVector = vector();
  PCVector = vector();
  APCVector = vector();
  RMVector = vector();
  RMRankit = vector()
  RMM = vector();
  POEDIRECTOR = vector();
  POEPercent = vector();
  POCVector = vector();
  AMDirector = vector();
  AMTime = vector();
  SizeVector = vector();
  SETVector = vector();
  
  for (index in 1:nrow(planilha11)) {
    actualCode <- planilha11$Código[index];
    actualYear <- planilha11$Ano[index];
    
    PJ <- filter(planilha1, planilha1$Código == actualCode & planilha1$Ano == actualYear );
    
    if (nrow(PRV) > 0) {
      PJVector <- c(PJVector, PJ$Número.Processos[1]);
    } else {
      PJVector <- c(PJVector, "");
    }
    
    PRV <- filter(planilha2, planilha2$Codigo == actualCode & planilha2$Ano == actualYear );
    
    if (nrow(PRV) > 0) {
      MCVector <- c(MCVector, PRV$Management.Council[1]);
      SDVector <- c(SDVector, PRV$Statutory.Directors[1]);
      FCVector <- c(FCVector, PRV$Fiscal.Council[1]);
    } else {
      MCVector <- c(MCVector, 0);
      SDVector <- c(SDVector, 0);
      FCVector <- c(FCVector, 0);
    }
    
    PC <- filter(planilha3, planilha3$Codigo == actualCode & planilha3$Ano == actualYear );
    
    if (nrow(PC) > 0) {
      PCVector <- c(PCVector, round(PC$Porcentagem.Conselheiros[1], 2));
    } else {
      PCVector <- c(PCVector, 0);
    }
    
    APC <- filter(planilha4, planilha4$Codigo == actualCode & planilha4$Ano == actualYear );
    
    if (nrow(APC) > 0) {
      APCVector <- c(APCVector, APC$APC[1]);
    } else {
      APCVector <- c(APCVector, 0);
    }
    
    RMdata <- filter(planilha6, planilha6$Codigo == actualCode & planilha6$Ano == actualYear );
    if (nrow(RMdata) > 0) {
      RMVector <- c(RMVector, round(RMdata$Remuneração.Média[1], 2));
      RMRankit <- c(RMRankit, RMdata$RMRankit[1]);
      RMM <- c(RMM, RMdata$RMM[1]);
    } else {
      RMVector <- c(RMVector, 0);
      RMRankit <- c(RMRankit, 0);
      RMM <- c(RMM, 0);
    }
    
    POE <- filter(planilha7, planilha7$Codigo == actualCode & planilha7$Ano == actualYear );
    
    if (nrow(POE) > 0) {
      directorValue = POE$Diretor[1];
      if (typeof(directorValue) != "character") {
        directorValue = levels(directorValue)[directorValue];
      }
      POEDIRECTOR <- c(POEDIRECTOR, directorValue);
      POEPercent <- c(POEPercent, round(POE$Percentual.de.Ações[1], 2));
    } else {
      POEDIRECTOR <- c(POEDIRECTOR, "");
      POEPercent <- c(POEPercent, 0);
    }
    
    POC <- filter(planilha8, planilha8$Código == actualCode & planilha8$Ano == actualYear );
    
    if (nrow(POC) > 0) {
      POCVector <- c(POCVector, POC$Dummy[1]);
    } else {
      POCVector <- c(POCVector, 0);
    }
    
    AM <- filter(planilha9, planilha9$Codigo == actualCode & planilha9$Ano == actualYear );
    
    if (nrow(AM) > 0) {
      directorValue = AM$Diretor[1];
      if (typeof(directorValue) != "character") {
        directorValue = levels(directorValue)[directorValue];
      }
      AMDirector <- c(AMDirector, directorValue);
      AMTime <- c(AMTime, AM$Tempo.de.Mandato..Anos.[1]);
    } else {
      AMDirector <- c(AMDirector, "");
      AMTime <- c(AMTime, 0);
    }
    
    SIZE <- filter(planilha10, planilha10$Código == actualCode & planilha10$Ano == actualYear )
    
    if (nrow(SIZE) > 0) {
      SizeVector <- c(SizeVector, round(SIZE$Valor.Ativo[1], 2));
    } else {
      SizeVector <- c(SizeVector, 0);
    }
    
    SET <- filter(planilha12, planilha12$Código == actualCode & planilha12$Ano == actualYear )
    
    if (nrow(SET) > 0) {
      setor = SET$Valor.Ativo[1];
      if (typeof(setor) != "character") {
        setor = levels(setor)[setor];
      }
      SETVector <- c(SETVector, setor);
    } else {
      SETVector <- c(SETVector, "");
    }
    
  }
  
  planilhaCompleta <- planilha11;
  colnames(planilhaCompleta)[5] = "NIV"
  planilhaCompleta["PJ"] = PJVector;
  planilhaCompleta["Management.Council"] = MCVector;
  planilhaCompleta["Statutory.Directors"] = SDVector;
  planilhaCompleta["Fiscal.Council"] = FCVector;
  planilhaCompleta["PC"] = PCVector;
  planilhaCompleta["APC"] = APCVector;
  planilhaCompleta["RM"] = RMVector;
  planilhaCompleta["RMRANKIT"] = RMRankit;
  planilhaCompleta["RMM"] = RMM;
  planilhaCompleta["POEPercent"] = POEPercent;
  planilhaCompleta["AM"] = AMTime;
  planilhaCompleta["PO"] = POCVector;
  planilhaCompleta["AMDIRECTOR"] = AMDirector;
  planilhaCompleta["AM"] = AMTime;
  planilhaCompleta["SIZE"] = SizeVector;
  planilhaCompleta["SETOR"] = SETVector;
  
  AIAE1 = vector();
  AIAE2 = vector();
  PE = vector();
  for (index in 1:nrow(planilhaCompleta)) {
    value = sum(planilhaCompleta$Management.Council[index],
                planilhaCompleta$Statutory.Directors[index],
                planilhaCompleta$Fiscal.Council[index],
                planilhaCompleta$PC[index],
                planilhaCompleta$APC[index]);
    vale = value - as.numeric(planilhaCompleta$PJ[index]);
    AIAE1 <- c(AIAE1, value);
    value = sum(planilhaCompleta$Management.Council[index],
                planilhaCompleta$Statutory.Directors[index],
                planilhaCompleta$Fiscal.Council[index],
                planilhaCompleta$APC[index]);
    value = value -as.numeric(planilhaCompleta$PJ[index]);
    AIAE2 = c(AIAE2, value);
    value = sum(planilhaCompleta$POEPercent[index],
                planilhaCompleta$PO[index],
                planilhaCompleta$AM[index]) ;
    PE = c(PE, round(value, 2));
    
  }
  planilhaCompleta["AIAE1"] = AIAE1;
  planilhaCompleta["AIAE2"] = AIAE2;
  planilhaCompleta["PE"] = PE;
  View(planilhaCompleta);
  write.xlsx2(planilhaCompleta, "./excels/PlanilhaCompleta.xlsx");
  
  return(planilhaCompleta)
}
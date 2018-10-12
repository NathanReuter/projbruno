# Exemple to fetch information from the info dataframe and rebuild into a dataframe again

getCompanyCode <- function(name) {
  code = unlist((filter(df.statements, company.name == name)[2]));
  if (length(code) == 0L) {
    return (666);
  }
  
  return (code);
}

# P1 - Número de processos judiciais sofridos pela empresa
# Pegar da tabela processos.xl, e padronizar por ano

p1 <- function (dataInfo) {
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
      codeVector[j + i] = getCompanyCode(company)[1];
      companyVector[j + i] <- company;
      processVector[j + i] <- yearInfo[[i, 2]];
      yearVector[j + i] <- year;
    }
  }
  
  resultFrame = data.frame(
    "Codigo" = codeVector, 
    "Companhia" = companyVector, "Número Processos" = processVector, "Ano" = yearVector);
  View(resultFrame)
  
  return (dataInfo);
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
        parsedYear = unlist(strsplit(toString(hComp$ref.date[index]), "-"))[1];
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
          parsedYear = parseDate(hComp$ref.date[index]);
          
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

#sellingPorcentageResultaData <-sellingPorcentage(df.statements)
# This writes into csv
#write.csv(sellingPorcentageResultaData, file = "./Planilha 1.csv")

#p4 pular
# Usar a 7 para saber quem são os CEO e verficar as pessoas fiísicas em history.stockholders, pegar todos menos o CEO.
# Depois pegar todas s pessoas fisicas e comparar quem faz parte do conselho em history.board.composition
# e somar as ações dessas pessoas

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
  View(resultFrame)
  return(resultFrame);
}

# p6 Ranking da Remuneração Média (RRM)
# Fazer Global, para todas as empresas e rankear entre 0-1 o as empresas de acrodo com a remuneração média

# p7 Nível percentual de ações em posse dos executivos
# Vasculhar history responsable e pegar os person.name com person.job == "Diretor Presidente"
# Depois usar o person.name e em history.stockholders, verificar se person.name == name.stockholder
# e captura o perc.ord.shares
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
  View(resultFrame)
  return(resultFrame);
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
      hResp = filter(hResp, person.job == "Diretor Presidente");
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
    "Dummy" = DummyVector
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


# p10 Logaritmo natural do total de ativos de uma empresa
# Pegar a planilha Bruno.xls
# Color no formato codigo, nome, e pegar o valor de total de ativos e aplica Ln em todos os anos(COlunas)
p10 <- function(dataInfo) {
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
      codeVector[i + j] = getCompanyCode(cName);
      companyVector[i + j] = cName;
      yearVector[i + j] = yearCounter;
      activeValueVector[i + j] = log(aValue);
    }
  }
  
  resultFrame = data.frame("Código" = codeVector, "Companhia" = companyVector, "Ano" = yearVector, 'Valor Ativo' = activeValueVector);
  View(resultFrame)
  
  return (resultFrame)
}

# p11 Dummy 1 - Novo Mercado ou Nível 2; 0 - Caso contrário
# history.gorvernance.listings
# listed.segment = "Novo Mercado" || "Nivel 2"
# Lista por ano e empresa

# p12 Dummy 1 - Setor que será observado; 0 - Caso contrário 
# Separa pela planilha bruno.xls código, nomeCOmpania e nomesetor
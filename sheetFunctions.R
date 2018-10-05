# Exemple to fetch information from the info dataframe and rebuild into a dataframe again
sellingPorcentage <- function(dataInfo) {
  #codigo da empresa, nome da empresa, ano, porcentage de renda variavel
  
  companyCode = dataInfo$company.code;
  companyName = dataInfo$company.name;
  
  # This will vary from list
  companyYear = list();
  variableIncomeList = dataInfo$fr.income;
  currentDate = list();
  valor = list();
  companyNameList = list();
  companyCodeList = list();
  
  for(collumn in variableIncomeList){
    currentDate = collumn[2];
    valor = collumn[5]
  }
  
  companyNameList[1: nrow(currentDate)] = companyName;
  companyCodeList[1: nrow(currentDate)] = companyCode;
  
  resultFrame = data.frame(listToROW(companyCodeList), listToROW(companyNameList), currentDate, valor);
  colnames(resultFrame) <- c("Código", "Empresa", "Ano", "Renda")
  
  return (resultFrame)
}

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
    "Compania" = companyVector, "Número Processos" = processVector, "Ano" = yearVector);
  View(resultFrame)
  
  return (dataInfo);
}

# Planilha 2 - Porcentagem em renda variável na remuneração dos executivos 
variableIncomePercentage <- function () {
  # get all variables. in history.compensation and divide to total.value.remunaration
  # And divid it in 3 columns Management Council, Statutory Directors, Fiscal Council
  # COLOCAR Data como apenas o Ano
  #hComp = df.statements['history.compensation'];
  #hComp = hComp[[1]];
}

# P3 - Porcentagem de conselheiros independentes
independentConselorPercentage <- function() {
  # Get code.type  == 27 in history.board.composition and divide by all ocurrences in the year
}

sellingPorcentageResultaData <-sellingPorcentage(df.statements)
# This writes into csv
#write.csv(sellingPorcentageResultaData, file = "./Planilha 1.csv")

#p4 pular
# Usar a 7 para saber quem são os CEO e verficar as pessoas fiísicas em history.stockholders, pegar todos menos o CEO.
# Depois pegar todas s pessoas fisicas e comparar quem faz parte do conselho em history.board.composition
# e somar as ações dessas pessoas

# p5 Remuneração Média (RM)
# pegar no history.compensation e dividir total.value / qtd.members

# p6 Ranking da Remuneração Média (RRM)
# Fazer Global, para todas as empresas e rankear entre 0-1 o as empresas de acrodo com a remuneração média

# p7 Nível percentual de ações em posse dos executivos
# Vasculhar history responsable e pegar os person.name com person.job == "Diretor Presidente"
# Depois usar o person.name e em history.stockholders, verificar se person.name == name.stockholder
# e captura o perc.ord.shares

# p8 Dummy 1 - CEO presidente do conselho; 0 - Caso contrário
# Varrer history.board.composition, e para cada ano se tiver cõdigo 30 dummy = 1 se nao 0

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
  
  resultFrame = data.frame("Código" = codeVector, "Compania" = companyVector, "Ano" = yearVector, 'Valor Ativo' = activeValueVector);
  View(resultFrame)
  
  return (resultFrame)
}

# p11 Dummy 1 - Novo Mercado ou Nível 2; 0 - Caso contrário
# history.gorvernance.listings
# listed.segment = "Novo Mercado" || "Nivel 2"
# Lista por ano e empresa

# p12 Dummy 1 - Setor que será observado; 0 - Caso contrário 
# Separa pela planilha bruno.xls código, nomeCOmpania e nomesetor
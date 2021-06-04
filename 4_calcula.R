### PENDENCIA NO LOOP
# Agora no RStudio cloud

## Momento 1
# 1) lancar os dados

# 2) executar o loop
# 3) salvar o resultados
# 4) repetir até o último resultado

# 5) salvar os resultados

# Pensar na situação em que não oocorre mais do que o momento1

## Momentos 2 e 3
# Executar
library(tidyverse)
library(lubridate)

## Banco de dados
options(warn = 1)
library(googlesheets4)
gs4_deauth()
#gs4_auth()


bd <- 'IndPrecVer1d'
raiz <- '/Users/fernandovieira/OneDrive/6. Alumni/gesALUMNI/Info/Versao 2/Em_Producao/4_momento1.R'
#raiz <- 'C:/Users/ferna/OneDrive/6. Alumni/gesALUMNI/Info/Versao 2/Em_Producao/4_calcAlumni_JM_SOURCE_17052021.R'
taxasDataMax <- dmy('31-03-2021') # automatizar isso depois

####  0. DADOS PROCESSO ####
## FORMULÁRIO 1

# Precatorio
requerente <- "SANDOVAL GOMES FERNANDES"
requerido <- "MUNICIPIO DE BARRA DO GARCAS"
vara <- "COMARCA DE BARRA DO GARÇAS - SDCR"
procTerc <- c("VERENA ANIZ SALOMAO (ADVOGADO)", "JOAO JAKSON VIEIRA GOMES (ADVOGADO)")

natureza <- ifelse(0, 
                   'Alimentar', 
                   'Comum/Ordinária')

precatorio <- "0079193-91.2017.8.11.0000"
protocolo <- "79193/2017"

calcProc <- 69235.31 # Apenas momento 2

ofReq <- "900269/2017"
reqDataPrec <- dmy("29-06-2017")

# Processo judicial
citacao <- dmy('01-01-1900') # (nao localizada: 01-01-1900)
julgado <- dmy("03-10-2012") # (nao localizada: 01-01-1900)

# Juros de mora
# (0 = Arrojado; 1 = Conservador; 2 = REsp 1272487)
# 0: FG ate inicio graca.; 1: citacao ate inicio graca; 2: 01/01 exerc. seguinte julgado ate inicio graca
jm_criterio <- 0

####  1. DADOS CÁLCULO (um) ####
# 1
nr <- 1

assign(paste('dfChave', nr, sep = ''), tibble(nomDataIn = dmy('15-01-2003'),
                                              nomDataFim = dmy('15-01-2003'),
                                              nominal = 247400)) -> dfChave

for (i in 1:nrow(dfChave)) {
  nomDataIn <- NA
  nomDataIn[i] <- dfChave$nomDataIn[i]
  nomDataIn <- as.Date(nomDataIn, origin = '1970-01-01')
  nomDataFim <- NA
  nomDataFim[i] <- dfChave$nomDataFim[i]
  nomDataFim <- as.Date(nomDataFim, origin = '1970-01-01')
  nominal <- NA
  nominal[i] <- dfChave$nominal[i]
  cmDataIn <- nomDataIn
  cmDataFim <- min(taxasDataMax,
                   dmy(paste(31, 12, year(reqDataPrec)+1, sep = '-'))) 
  
  source(raiz)
  
  assign(paste('nominal', nr, sep = ''), nominal); Lnominal <- get(as.character(as.name(paste('nominal', nr, sep = ''))))
  assign(paste('tReal', nr, sep = ''), tReal); LtReal <-  get(as.character(as.name(paste('tReal', nr, sep = ''))))
  assign(paste('tJM', nr, sep = ''), tJM); LtJM <-  get(as.character(as.name(paste('tJM', nr, sep = ''))))
  assign(paste('t', nr, sep = ''), t); Lt <-  get(as.character(as.name(paste('t', nr, sep = ''))))
  
  assign(paste('anexo1_', nr, sep = ''), anexo1(nr))
}

####  1. DADOS CÁLCULO (um) ####
# 2
nr <- nr+1

assign(paste('dfChave', nr, sep = ''), tibble(nomDataIn = dmy('15-02-2003'),
                                              nomDataFim = dmy('15-02-2003'),
                                              nominal = 100)) -> dfChave

for (i in 1:nrow(dfChave)) {
  nomDataIn <- NA
  nomDataIn[i] <- dfChave$nomDataIn[i]
  nomDataIn <- as.Date(nomDataIn, origin = '1970-01-01')
  nomDataFim <- NA
  nomDataFim[i] <- dfChave$nomDataFim[i]
  nomDataFim <- as.Date(nomDataFim, origin = '1970-01-01')
  nominal <- NA
  nominal[i] <- dfChave$nominal[i]
  cmDataIn <- nomDataIn
  cmDataFim <- min(taxasDataMax,
                   dmy(paste(31, 12, year(reqDataPrec)+1, sep = '-'))) 
  source(raiz)
}

assign(paste('nominal', nr, sep = ''), nominal); Lnominal <- append(Lnominal, get(as.character(as.name(paste('nominal', nr, sep = '')))))
assign(paste('tReal', nr, sep = ''), tReal); LtReal <-  append(LtReal, get(as.character(as.name(paste('tReal', nr, sep = '')))))
assign(paste('tJM', nr, sep = ''), tJM); LtJM <-  append(LtJM, get(as.character(as.name(paste('tJM', nr, sep = '')))))
assign(paste('t', nr, sep = ''), t); Lt <-  append(Lt, get(as.character(as.name(paste('t', nr, sep = '')))))

assign(paste('anexo1_', nr, sep = ''), anexo1(nr))

#### 2. SOMAR RESULTADOS ####

dfMomento1 <- tibble(nominal = sum(Lnominal),
                     tReal = sum(LtReal),
                     tJM = sum(LtJM),
                     t = sum(Lt))

momentos <- tibble(n = seq(1:length(Lt)),
                   `Pagto. inicial` = c(dfChave1$nomDataIn, dfChave2$nomDataIn),
                   `Pagto. final` = c(dfChave1$nomDataFim, dfChave2$nomDataFim),
                   `Nr. pagtos` = c(1, 1), # resolver isso depois
                   `Valor unitário (R$)` = c(nominal1, nominal2),
                   `Corrigido (R$)` = c(tReal1, tReal2),
                   `JM (R$)` = c(tJM1, tJM2),
                   `Total (R$)` = c(t1, t2))

# momentos <- tibble(n = seq(1:length(Lt)),
#                    `Pagto. inicial` = c(dfChave1$nomDataIn),#, dfChave2$nomDataIn),
#                    `Pagto. final` = c(dfChave1$nomDataFim),#, dfChave2$nomDataFim),
#                    `Nr. pagtos` = c(1), # resolver isso depois
#                    `Valor unitário (R$)` = c(nominal1),#, nominal2),
#                    `Corrigido (R$)` = c(tReal1),#, tReal2),
#                    `JM (R$)` = c(tJM1),#, tJM2),
#                    `Total (R$)` = c(t1))#, t2))

momentos$`Pagto. inicial` <- paste(month(momentos$`Pagto. inicial`), '/', year(momentos$`Pagto. inicial`), sep = '')
momentos$`Pagto. final` <- paste(month(momentos$`Pagto. final`), '/', year(momentos$`Pagto. final`), sep = '')
momentos <- momentos %>% mutate('Nominal (R$)' = `Nr. pagtos`*`Valor unitário (R$)`) %>% relocate('Nominal (R$)', .after = `Valor unitário (R$)`)

momentos <- momentos %>% adorn_totals('row',  na.rm = TRUE
                                      ,,,c('Nr. pagtos', 'Nominal (R$)', 'Corrigido (R$)', 'JM (R$)', 'Total (R$)'))
                   
#### Tabela MOMENTO 1 ####
tab1 <- format(as.data.frame(momentos), decimal.mark = ',', big.mark = '.') %>%
  kable('html', escape = F) %>%
  
  # Linha 
  add_header_above(c('', paste(format(as.Date(dfChave1$nomDataIn), '%d/%m/%Y'), 'a', format(as.Date(cmDataFim), '%d/%m/%Y')), 
                     '', '', '', '', '', 
                     c(paste(format(as.Date(dfChave1$nomDataIn), '%d/%m/%Y'), 'a', format(as.Date(jrsMorDataFim), '%d/%m/%Y'))), ''),

                   bold = FALSE, color = 'gray', line = FALSE, font_size = 'small', escape = TRUE) %>%
  # Linha
  add_header_above(c('', 'Correção Monetária:', '', '', '', '', '', 'Juros de Mora:', ''), 
                   bold = FALSE, color = 'gray', line = FALSE, font_size = 'small') %>%
  # Titulo
  add_header_above(c('', 'MOMENTO 1 (período 1):\nProcesso judicial, precatório e graça (CF, art. 100, § 5º): \n Datas, valores, correção e juros' = 8),
                   line = FALSE) %>%
  # Formatacao tabela
  kable_minimal(html_font = 'cambria', font_size = 14)

tab1

## Parei aqui: estudar a questao de quando o pedido foi feito há menos tempo do que a contagem dos momentos 2 e 3 

#### Fim: MOMENTO 1 @ ####

#### Inicio: MOMENTO 2 @@ ####
# Corrigido (Per. 1)

#### { ####
#### ******************************************************************************** ####
#### 0. CONFIGURAR AMBIENTE ####
#### ******************************************************************************** ####

#### { ####
#### ******************************************************************************** ####
####  PARTE I: Definir variáveis, valores chave e encontrar o VALOR nominal ####
#### ******************************************************************************** ####

####  0. DADOS PROCESSO ####

# Valor base
nominal <- dfMomento1$tReal # (fls. 15) # <>>>

####  1. DADOS CÁLCULO ####
#### 1.1 Data(s) base ####

# NOMINAL (principal)
nomDataIn <- cmDataFim+1 # <>>>
nomDataFim <- cmDataFim+1 # <>>>

# CORRECAO MONETARIA
cmDataIn <- nomDataIn
cmDataFim <- dmy('31-03-2021')

## JUROS

# Juros de mora
jrsMorDataIn <- cmDataIn
jrsMorDataFim <- cmDataFim

#### 1.2 Intervalos ####

# Principal
nomDataInterv <- if (day(nomDataIn) > day(nomDataFim)) {
  interval(nomDataIn, nomDataFim) %/% months(1) + 2
}  else if (day(nomDataFim) >= day(nomDataIn)) {
  interval(nomDataIn, nomDataFim) %/% months(1) + 1 
} else {
  interval(nomDataIn, nomDataFim) %/% months(1) + 1
} 

# Correcao monetaria
cmInterv <- if (day(cmDataIn) > day(cmDataFim)) {
  interval(cmDataIn, cmDataFim) %/% months(1) + 2
}  else if (day(cmDataFim) >= day(cmDataIn)) {
  interval(cmDataIn, cmDataFim) %/% months(1) + 1 
} else {
  interval(cmDataIn, cmDataFim) %/% months(1) + 1
}

# Juros de mora
jrsMorInterv <- if (day(jrsMorDataIn) > day(jrsMorDataFim)) {
  interval(jrsMorDataIn, jrsMorDataFim) %/% months(1) + 2
}  else if (day(jrsMorDataFim) >= day(jrsMorDataIn)) {
  interval(jrsMorDataIn, jrsMorDataFim) %/% months(1) + 1 
} else {
  interval(jrsMorDataIn, jrsMorDataFim) %/% months(1) + 1
}

#### 1.3 Tempos para contagem ####

# Correção monetaria
ncm <- tibble(ncm = cmInterv:(cmInterv-nomDataInterv+1))

# Juros de mora
njrMor <- tibble(njrMor = jrsMorInterv:(jrsMorInterv-nomDataInterv+1))
njrMor <- filter(njrMor, njrMor>0)

#### 1.4 Índice(s) e Juros ####

#### 1.5 Ajustes ####

#### 1.5.1 Periodo dos calculos ####
indices <- if (day(cmDataIn) != 1) {
  filter(indicesBrutos, Mes >= cmDataIn-30, Mes <= cmDataFim)
} else {
  filter(indicesBrutos, Mes >= cmDataIn, Mes <= cmDataFim)
}

### Correcao monetaria (CM)

## Pagto inicial (CM)
# Data
indices$Mes[1] <- cmDataIn

# Pro-rata inicial (CM)
indices$VarPerc[1] <- if (day(cmDataIn) != 1 & day(cmDataIn) != 30 & day(cmDataIn) != 31) {
  indices$VarPerc[1] = (indices$VarPerc[1] / 30) * (30-day(cmDataIn))
} else {
  indices$VarPerc[1] = indices$VarPerc[1]
}

# Fator (CM)
indices$Fator[1] <- 1 + (indices$VarPerc[1] / 100)

## Pagto final (CM)
# Data
indices$Mes[cmInterv] <- cmDataFim

# Pro-rata final (CM)
indices$VarPerc[cmInterv] <- if (day(cmDataFim) != 1 & day(cmDataFim) != 30 & day(cmDataFim) != 31) {
  indices$VarPerc[cmInterv] = (indices$VarPerc[cmInterv] / 30) * (day(cmDataFim))
} else {
  indices$VarPerc[cmInterv] = indices$VarPerc[cmInterv]
}

# Fator (CM)
indices$Fator[cmInterv] <- 1 + (indices$VarPerc[cmInterv] / 100)

### Juros de mora (JM)

## Pagto inicial (JM)
# Data inicial (JM)
indices$Mes[which(indices$Mes==paste(year(jrsMorDataIn), if_else(month(jrsMorDataIn)<=09, (gsub(' ','', paste('0', month(as.Date(strftime(jrsMorDataIn, format = '%Y-%m-%d'))), collapse = '/'))), as.character(month(as.Date(strftime(jrsMorDataIn, format = '%Y-%m-%d'))))), '01', sep = '-'))] <- jrsMorDataIn

# Pro-rata inicial (JM)
indices$JMMes[which(indices$Mes==jrsMorDataIn)] <- if (day(jrsMorDataIn) != 1 & day(jrsMorDataIn) != 30 & day(jrsMorDataIn) != 31) {
  indices$JMMes[which(indices$Mes==jrsMorDataIn)] = (indices$JMMes[which(indices$Mes==jrsMorDataIn)] / 30) * (30-day(jrsMorDataIn))
} else {
  indices$JMMes[which(indices$Mes==jrsMorDataIn)] = indices$JMMes[which(indices$Mes==jrsMorDataIn)]
}

# Pro-rata final (JM)
# Data final (JM)
indices$Mes[which(indices$Mes==paste(year(jrsMorDataFim), if_else(month(jrsMorDataFim)<=09, (gsub(' ','', paste('0', month(as.Date(strftime(jrsMorDataFim, format = '%Y-%m-%d'))), collapse = '/'))), as.character(month(as.Date(strftime(jrsMorDataFim, format = '%Y-%m-%d'))))), '01', sep = '-'))] <- jrsMorDataFim

# Pro-rata final (JM)
indices$JMMes[which(indices$Mes==jrsMorDataFim)] <- if (day(jrsMorDataFim) != 1 & day(jrsMorDataFim) != 30 & day(jrsMorDataFim) != 31) {
  indices$JMMes[which(indices$Mes==jrsMorDataFim)] = (indices$JMMes[which(indices$Mes==jrsMorDataFim)] / 30) * (day(jrsMorDataFim))
} else {
  indices$JMMes[which(indices$Mes==jrsMorDataFim)] = indices$JMMes[which(indices$Mes==jrsMorDataFim)]
}

# Fator
# Juros nao tem fator, pois sao calculados como simples

# Processo
processo <- indices %>%
  mutate(Fac = cumprod(Fator), 
         JMAcum = cumsum(JMMes))

#### ******************************************************************************** ####
#### 2. VALOR(ES) nominal(IS) ####
# Pode ser: i) valor unico; ou ii) mais de um valor

#### 2.1 nominal ####
Totalnominal <- as.numeric(nominal * as.integer(nomDataInterv))
print(format(Totalnominal, scientific = FALSE))

#### 2.2 Data Frame ####
Dfnominal <- as.data.frame(rep(nominal, nomDataInterv))
colnames(Dfnominal) <- 'nominal'

xnaNom <- if (nrow(processo) != nrow(ncm)) {
  rep(NA, nrow(processo) - nrow(ncm)) # Adicionando valores NA
}

xnaJM <- if (nrow(processo) != nrow(njrMor)) {
  rep(NA, nrow(processo) - nrow(njrMor)) # Adicionando valores NA
}

Dfnominal <- add_row(Dfnominal, nominal = xnaNom)
Dfnominal <- as_tibble(Dfnominal)

#### 2.3 n Meses CM e Juros ####
## Correcao monetaria
nCM <- add_row(ncm, ncm = xnaNom)
nCM <- as_tibble(nCM)

## Juros de mora (JM)
nJRMOR <- add_row(njrMor, njrMor = xnaJM)
nJRMOR  <- as_tibble(nJRMOR)

# Processo
processo <- processo %>% 
  mutate(nominal = Dfnominal$nominal)

processo <- processo %>%
  mutate(nominalAcum = cumsum(processo$nominal),
         nCM = nCM$ncm,
         nJRMOR = nJRMOR$njrMor,
         n = seq(1:length(Mes))) %>%
  select(n, everything())

#### } ####

#### { ####
#### ******************************************************************************** ####
####  PARTE II: Encontrar CORREÇAO MONETARIA e JUROS ####
#### ******************************************************************************** ####

#### 3. CORREÇAO MONETARIA ####
#### 3.1 Fator k ####
## Encontrar o valor k (maior Fac) de CM acumulada, de acordo com o numero de meses para correçao.
# Servira de referencia para o calculo da cm dos valores mensais.
k <- processo %>%
  select(Fac) %>%
  slice(n()) %>% # slice(n()) seleciona a ultima linha da coluna Fac.
  as.numeric()

#### 3.2 Correcao nominal ####
Corrnominal <- processo %>%
  filter(Mes >= as.Date(cmDataIn), Mes <= as.Date(cmDataFim)) %>%
  mutate(Real = k * nominal / Fac) %>%
  select(Real)

TotalReal <- round(colSums(Corrnominal, na.rm = TRUE), 2)

# Processo
processo <- processo %>%
  mutate(Corrigido = Corrnominal$Real,
         CorrigidoAcum = round(cumsum(Corrnominal$Real), 2))

#### ******************************************************************************** ####
#### 4. JUROS ####
#### 4.1 Fatores j ####
## Encontrar o valor j (maior Fac) de JM acumulado, de acordo com o numero de meses para correcao

# JM
jm <- processo %>% select(nJRMOR) %>% na.omit()

for (i in jm) {
  jm <- data.frame(cbind(processo$JMAcum[i]), check.names = FALSE)
  jm <- as_tibble(jm) %>% rename(nJRMORFac = 'cbind(processo$JMAcum[i])')
  jm <- add_row(jm, nJRMORFac = xnaJM)
}

#### 4.2 Juro nominal ####

# JM
processo <- processo %>% 
  mutate(JMFac = jm$nJRMORFac) %>%
  select(n, Mes, nominal, nominalAcum, 
         nCM, Indice, Fator, Fac, Corrigido, CorrigidoAcum, 
         nJRMOR, JMMes, JMFac, JMAcum) %>%
  mutate(JurosMora = Corrigido * JMFac, 
         JurosMoraAcum = cumsum(JurosMora)) %>%
  mutate(Total = Corrigido+JurosMora, 
         TotalAcum = cumsum(Total))

#### } ####

#### { ####
#### ******************************************************************************** ####
####  PARTE III: Resumir o processo e imprimir tabelas e graficos ####
#### ******************************************************************************** ####

#### 5. PROCESSO FINAL ####

#### nominal ####
# Ou principal
tNominal <- Totalnominal

#### Real ####
# Ou corrigido
tReal <- TotalReal -> tReal2 # Valor atualizado

##### Juros de Mora (JM) ####
tJM <- processo %>% select(JurosMoraAcum) %>% na.omit() %>% tail(1) %>% as.numeric() %>% round(2) -> tJM2

##### __ Total ####
# Corrigido + Juros
t <- tReal + tJM

#### Tabela MOMENTO 2 ####
options(digits=12)
matrizDf2 <- matrix(c(tNominal, tReal, tJM, t), 1,4, dimnames = list(c(''),
                                                                     c('Nominal (R$)', 'Corrigido (R$)', 'JM (R$)', 'Total (R$)')))

# Inicio do codigo da tabela 2
tab2 <- format(as.data.frame(matrizDf2), decimal.mark = ',', big.mark = '.') %>%
  kable('html', escape = FALSE) %>%
  # Linha 
  add_header_above(c('', paste(format(as.Date(cmDataIn), '%d/%m/%Y'), 'a', format(as.Date(cmDataFim), '%d/%m/%Y')), 
                     '', '', 
                     c(paste(format(as.Date(cmDataIn), '%d/%m/%Y'), 'a', format(as.Date(cmDataFim), '%d/%m/%Y')))), 
                   bold = FALSE, color = 'gray', line = FALSE, font_size = 'small', escape = TRUE) %>%
  # Linha 
  add_header_above(c('', 'Correção Monetária:', '', '', 'Juros de Mora:'), 
                   bold = FALSE, color = 'gray', line = FALSE, font_size = 'small') %>%
  # Titulo
  add_header_above(c('', 'MOMENTO 2 (período 2):\nPós-graça / momento atual (CF, art. 100, § 5º): \n Datas, valores, correção e juros' = 4),
                   line = FALSE) %>%
  # Formatacao tabela
  kable_minimal(html_font = 'cambria', font_size = 14)

tab2

#### ******************************************************************************** ####
#### 6. IMPRESSAO ####
# Dados para Calculo
dfProc <- as.data.frame(processo)

dfProc <- dfProc %>% select(c('n', 'Mes',
                              'nominal', 'nominalAcum',
                              'nCM', 'Indice', 'Fator', 'Fac', 'Corrigido', 'CorrigidoAcum',
                              'nJRMOR', 'JMMes', 'JMFac', 'JMAcum', 'JurosMora', 'JurosMoraAcum',
                              'Total', 'TotalAcum'))

dfProc[,3:4] <- round(dfProc[,3:4], 2)
dfProc[,7:8] <- round(dfProc[,7:8], 6)
dfProc[,9:10] <- round(dfProc[,9:10], 2)
dfProc[,12:14] <- round(dfProc[,12:14], 6)
dfProc[,15:16] <- round(dfProc[,15:16], 2)
dfProc[,17:18] <- round(dfProc[,17:18], 2)

names(dfProc)[1] <- 'n'
names(dfProc)[2] <- 'Mês'

names(dfProc)[3] <- 'nominal (R$)'
names(dfProc)[4] <- 'nominal Acum. (R$)'

names(dfProc)[5] <- 'nº CM'
names(dfProc)[6] <- 'Índice'
names(dfProc)[7] <- 'Taxa Índice (mês)'
names(dfProc)[8] <- 'Fator CM (fac)'
names(dfProc)[9] <- 'Corrigido (R$)'
names(dfProc)[10] <- 'Corrigido Acum. (R$)'

names(dfProc)[11] <- 'nº JM (meses)'
names(dfProc)[12] <- 'Taxa JM (mês)'
names(dfProc)[13] <- 'Fator JM (fac)'
names(dfProc)[14] <- 'Taxa JM Acum.'
names(dfProc)[15] <- 'JM (R$)'    
names(dfProc)[16] <- 'JM Acum. (R$)'

names(dfProc)[17] <- 'Total (R$)'
names(dfProc)[18] <- 'Total Acum. (R$)'

dfProc$Mês = format(as.Date(dfProc$Mês), '%d/%m/%Y')

#### IMPRESSAO ####
evento2 <- 'MEMÓRIA DE CÁLCULO | Momento 2 | Período 2'

anexo2 <- 
  # CABEÇALHO
  format(as.data.frame(dfProc), decimal.mark = ',', big.mark = '.') %>%
  kable(align = 'r', caption = evento2, 'html') %>%
  add_header_above(c(rep('', 18))) %>%
  
  # linha 5
  add_header_above(c(rep('', 4), 
                     'Nº de meses (NM)', nomDataInterv,
                     cmInterv,
                     jrsMorInterv,
                     rep('', 10)), font_size = 10, line = FALSE, align = 'r', bold = FALSE, color = 'gray') %>%
  # linha 4
  add_header_above(c(rep('', 4), 
                     'Final', format(as.Date(nomDataFim), '%d/%m/%Y'), 
                     format(as.Date(cmDataFim), '%d/%m/%Y'),
                     format(as.Date(jrsMorDataFim), '%d/%m/%Y'),
                     rep('', 10)), 
                   font_size = 10, line = FALSE, align = 'r', bold = FALSE, color = 'gray') %>%
  # linha 3
  add_header_above(c(rep('', 4),
                     'Inicial', format(as.Date(nomDataIn), '%d/%m/%Y'), 
                     format(as.Date(cmDataIn), '%d/%m/%Y'),
                     format(as.Date(jrsMorDataIn), '%d/%m/%Y'),
                     rep('', 2),
                     format(as.numeric(tNominal), decimal.mark = ',', big.mark = '.'),
                     format(as.numeric(tReal), decimal.mark = ',', big.mark = '.'),
                     format(as.numeric(tJM), decimal.mark = ',', big.mark = '.'),
                     format(as.numeric(t), decimal.mark = ',', big.mark = '.'),
                     rep('', 4)),
                   font_size = 10, line = FALSE, align = 'r', bold = FALSE, color = 'gray') %>%
  
  # linha 2
  add_header_above(c(rep('', 4),
                     'DATA',
                     'VALOR NOMINAL', 
                     'CM', 
                     'JM',
                     rep('', 2),
                     'NOMINAL', 
                     'CM', 
                     'JM', 
                     'TOTAL',
                     rep('', 4)),
                   font_size = 10, line = FALSE, align = 'r') %>%
  # linha 1
  add_header_above(c(rep('', 4),
                     'TEMPOS' = 4,
                     rep('', 2),
                     'MONTANTES (R$)' = 4,
                     rep('', 4)),
                   font_size = 10, line = TRUE, align = 'c') %>%
  
  # RODAPÉ
  kable_styling(font_size = 10)

anexo2

#### Fim: MOMENTO 2 @@ ####

#### Inicio: MOMENTO 3A @@@A ####
# Juros de Mora (Per. 1)

#### { ####
#### ******************************************************************************** ####
#### 0. CONFIGURAR AMBIENTE ####
#### ******************************************************************************** ####

#### { ####
#### ******************************************************************************** ####
####  PARTE I: Definir variáveis, valores chave e encontrar o VALOR nominal ####
#### ******************************************************************************** ####

####  0. DADOS PROCESSO ####

# Valor base
nominal <- tJM1 # JM1 (se for mais de um pagto no momento 1, mudar o valor manualmente)

####  1. DADOS CÁLCULO ####
#### 1.1 Data(s) base ####

# NOMINAL (principal)
nomDataIn <- nomDataIn # <>>>
nomDataFim <- nomDataFim # <>>>

# CORRECAO MONETARIA
cmDataIn <- cmDataIn -> cmDataIn3A
cmDataFim <- cmDataFim -> cmDataFim3A

## JUROS

# Juros de mora
jrsMorDataIn <- jrsMorDataIn
jrsMorDataFim <- jrsMorDataFim

#### 1.2 Intervalos ####

# Principal
nomDataInterv <- if (day(nomDataIn) > day(nomDataFim)) {
  interval(nomDataIn, nomDataFim) %/% months(1) + 2
}  else if (day(nomDataFim) >= day(nomDataIn)) {
  interval(nomDataIn, nomDataFim) %/% months(1) + 1 
} else {
  interval(nomDataIn, nomDataFim) %/% months(1) + 1
} 

# Correcao monetaria
cmInterv <- if (day(cmDataIn) > day(cmDataFim)) {
  interval(cmDataIn, cmDataFim) %/% months(1) + 2
}  else if (day(cmDataFim) >= day(cmDataIn)) {
  interval(cmDataIn, cmDataFim) %/% months(1) + 1 
} else {
  interval(cmDataIn, cmDataFim) %/% months(1) + 1
}

# Juros de mora
jrsMorInterv <- if (day(jrsMorDataIn) > day(jrsMorDataFim)) {
  interval(jrsMorDataIn, jrsMorDataFim) %/% months(1) + 2
}  else if (day(jrsMorDataFim) >= day(jrsMorDataIn)) {
  interval(jrsMorDataIn, jrsMorDataFim) %/% months(1) + 1 
} else {
  interval(jrsMorDataIn, jrsMorDataFim) %/% months(1) + 1
}

#### 1.3 Tempos para contagem ####

# Correção monetaria
ncm <- tibble(ncm = cmInterv:(cmInterv-nomDataInterv+1))

# Juros de mora
njrMor <- tibble(njrMor = jrsMorInterv:(jrsMorInterv-nomDataInterv+1))
njrMor <- filter(njrMor, njrMor>0)

#### 1.5 Ajustes ####

#### 1.5.1 Periodo dos calculos ####
indices <- if (day(cmDataIn) != 1) {
  filter(indicesBrutos, Mes >= cmDataIn-30, Mes <= cmDataFim)
} else {
  filter(indicesBrutos, Mes >= cmDataIn, Mes <= cmDataFim)
}

### Correcao monetaria (CM)

## Pagto inicial (CM)
# Data
indices$Mes[1] <- cmDataIn

# Pro-rata inicial (CM)
indices$VarPerc[1] <- if (day(cmDataIn) != 1 & day(cmDataIn) != 30 & day(cmDataIn) != 31) {
  indices$VarPerc[1] = (indices$VarPerc[1] / 30) * (30-day(cmDataIn))
} else {
  indices$VarPerc[1] = indices$VarPerc[1]
}

# Fator (CM)
indices$Fator[1] <- 1 + (indices$VarPerc[1] / 100)

## Pagto final (CM)
# Data
indices$Mes[cmInterv] <- cmDataFim

# Pro-rata final (CM)
indices$VarPerc[cmInterv] <- if (day(cmDataFim) != 1 & day(cmDataFim) != 30 & day(cmDataFim) != 31) {
  indices$VarPerc[cmInterv] = (indices$VarPerc[cmInterv] / 30) * (day(cmDataFim))
} else {
  indices$VarPerc[cmInterv] = indices$VarPerc[cmInterv]
}

# Fator (CM)
indices$Fator[cmInterv] <- 1 + (indices$VarPerc[cmInterv] / 100)

### Juros de mora (JM)

## Pagto inicial (JM)
# Data inicial (JM)
indices$Mes[which(indices$Mes==paste(year(jrsMorDataIn), if_else(month(jrsMorDataIn)<=09, (gsub(' ','', paste('0', month(as.Date(strftime(jrsMorDataIn, format = '%Y-%m-%d'))), collapse = '/'))), as.character(month(as.Date(strftime(jrsMorDataIn, format = '%Y-%m-%d'))))), '01', sep = '-'))] <- jrsMorDataIn

# Pro-rata inicial (JM)
indices$JMMes[which(indices$Mes==jrsMorDataIn)] <- if (day(jrsMorDataIn) != 1 & day(jrsMorDataIn) != 30 & day(jrsMorDataIn) != 31) {
  indices$JMMes[which(indices$Mes==jrsMorDataIn)] = (indices$JMMes[which(indices$Mes==jrsMorDataIn)] / 30) * (30-day(jrsMorDataIn))
} else {
  indices$JMMes[which(indices$Mes==jrsMorDataIn)] = indices$JMMes[which(indices$Mes==jrsMorDataIn)]
}

# Pro-rata final (JM)
# Data final (JM)
indices$Mes[which(indices$Mes==paste(year(jrsMorDataFim), if_else(month(jrsMorDataFim)<=09, (gsub(' ','', paste('0', month(as.Date(strftime(jrsMorDataFim, format = '%Y-%m-%d'))), collapse = '/'))), as.character(month(as.Date(strftime(jrsMorDataFim, format = '%Y-%m-%d'))))), '01', sep = '-'))] <- jrsMorDataFim

# Pro-rata final (JM)
indices$JMMes[which(indices$Mes==jrsMorDataFim)] <- if (day(jrsMorDataFim) != 1 & day(jrsMorDataFim) != 30 & day(jrsMorDataFim) != 31) {
  indices$JMMes[which(indices$Mes==jrsMorDataFim)] = (indices$JMMes[which(indices$Mes==jrsMorDataFim)] / 30) * (day(jrsMorDataFim))
} else {
  indices$JMMes[which(indices$Mes==jrsMorDataFim)] = indices$JMMes[which(indices$Mes==jrsMorDataFim)]
}

# Fator
# Juros nao tem fator, pois sao calculados como simples

# Processo
processo <- indices %>%
  mutate(Fac = cumprod(Fator), 
         JMAcum = cumsum(JMMes))

#### ******************************************************************************** ####
#### 2. VALOR(ES) nominal(IS) ####
# Pode ser: i) valor unico; ou ii) mais de um valor

#### 2.1 nominal ####
Totalnominal <- as.numeric(nominal * as.integer(nomDataInterv))
print(format(Totalnominal, scientific = FALSE))

#### 2.2 Data Frame ####
Dfnominal <- as.data.frame(rep(nominal, nomDataInterv))
colnames(Dfnominal) <- 'nominal'

xnaNom <- if (nrow(processo) != nrow(ncm)) {
  rep(NA, nrow(processo) - nrow(ncm)) # Adicionando valores NA
}

xnaJM <- if (nrow(processo) != nrow(njrMor)) {
  rep(NA, nrow(processo) - nrow(njrMor)) # Adicionando valores NA
}

Dfnominal <- add_row(Dfnominal, nominal = xnaNom)
Dfnominal <- as_tibble(Dfnominal)

#### 2.3 n Meses CM e Juros ####
## Correcao monetaria
nCM <- add_row(ncm, ncm = xnaNom)
nCM <- as_tibble(nCM)

## Juros de mora (JM)
nJRMOR <- add_row(njrMor, njrMor = xnaJM)
nJRMOR  <- as_tibble(nJRMOR)

# Processo
processo <- processo %>% 
  mutate(nominal = Dfnominal$nominal)

processo <- processo %>%
  mutate(nominalAcum = cumsum(processo$nominal),
         nCM = nCM$ncm,
         nJRMOR = nJRMOR$njrMor,
         n = seq(1:length(Mes))) %>%
  select(n, everything())

#### } ####

#### { ####
#### ******************************************************************************** ####
####  PARTE II: Encontrar CORREÇAO MONETARIA e JUROS ####
#### ******************************************************************************** ####

#### 3. CORREÇAO MONETARIA ####
#### 3.1 Fator k ####
## Encontrar o valor k (maior Fac) de CM acumulada, de acordo com o numero de meses para correçao.
# Servira de referencia para o calculo da cm dos valores mensais.
k <- processo %>%
  select(Fac) %>%
  slice(n()) %>% # slice(n()) seleciona a ultima linha da coluna Fac.
  as.numeric()

#### 3.2 Correcao nominal ####
Corrnominal <- processo %>%
  filter(Mes >= as.Date(cmDataIn), Mes <= as.Date(cmDataFim)) %>%
  mutate(Real = k * nominal / Fac) %>%
  select(Real)

TotalReal <- round(colSums(Corrnominal, na.rm = TRUE), 2)

# Processo
processo <- processo %>%
  mutate(Corrigido = Corrnominal$Real,
         CorrigidoAcum = round(cumsum(Corrnominal$Real), 2))

#### ******************************************************************************** ####
#### 4. JUROS ####
#### 4.1 Fatores j ####
## Encontrar o valor j (maior Fac) de JM acumulado, de acordo com o numero de meses para correcao

# JM
jm <- processo %>% select(nJRMOR) %>% na.omit()

for (i in jm) {
  jm <- data.frame(cbind(processo$JMAcum[i]), check.names = FALSE)
  jm <- as_tibble(jm) %>% rename(nJRMORFac = 'cbind(processo$JMAcum[i])')
  jm <- add_row(jm, nJRMORFac = xnaJM)
}

#### 4.2 Processo final ####
processo <- processo %>% 
  mutate(JMFac = jm$nJRMORFac) %>%
  select(n, Mes, nominal, nominalAcum, 
         nCM, Indice, Fator, Fac, Corrigido, CorrigidoAcum, 
         nJRMOR, JMMes, JMFac, JMAcum) %>%
  mutate(JurosMora = Corrigido * JMFac, 
         JurosMoraAcum = cumsum(JurosMora)) %>%
  mutate(Total = Corrigido+JurosMora, 
         TotalAcum = cumsum(Total))

#### } ####

#### { ####
#### ******************************************************************************** ####
####  PARTE III: Resumir o processo e imprimir tabelas e graficos ####
#### ******************************************************************************** ####

#### 5. PROCESSO FINAL ####

#### 5.1 Tibble Processo Impressao ####
# Selecionando apenas as colunas que serao impressas
# processoIMP <- processo
# processoIMP

#### 5.2 Resumo ####
# t = TOTAL

#### nominal ####
# Ou principal
tNominal <- Totalnominal -> tNominal3A

#### Real ####
# Ou corrigido
tReal <- TotalReal -> tReal3A # Valor atualizado

##### Juros de Mora (JM) ####
tJM <- processo %>% select(JurosMoraAcum) %>% na.omit() %>% tail(1) %>% as.numeric() %>% round(2) -> tJM3A 

##### __ Total ####
# Corrigido + Juros
t <- tReal + tJM -> t3A

#### __ Diferencas ####
# Real - nominal
tDif <- t - tNominal

#### ******************************************************************************** ####
#### 6. IMPRESSAO ####
# Dados para Calculo
dfProc <- as.data.frame(processo)

dfProc <- dfProc %>% select(c('n', 'Mes',
                              'nominal', 'nominalAcum',
                              'nCM', 'Indice', 'Fator', 'Fac', 'Corrigido', 'CorrigidoAcum',
                              'nJRMOR', 'JMMes', 'JMFac', 'JMAcum', 'JurosMora', 'JurosMoraAcum',
                              'Total', 'TotalAcum'))

dfProc[,3:4] <- round(dfProc[,3:4], 2)
dfProc[,7:8] <- round(dfProc[,7:8], 6)
dfProc[,9:10] <- round(dfProc[,9:10], 2)
dfProc[,12:14] <- round(dfProc[,12:14], 6)
dfProc[,15:16] <- round(dfProc[,15:16], 2)
dfProc[,17:18] <- round(dfProc[,17:18], 2)

names(dfProc)[1] <- 'n'
names(dfProc)[2] <- 'Mês'

names(dfProc)[3] <- 'nominal (R$)'
names(dfProc)[4] <- 'nominal Acum. (R$)'

names(dfProc)[5] <- 'nº CM'
names(dfProc)[6] <- 'Índice'
names(dfProc)[7] <- 'Taxa Índice (mês)'
names(dfProc)[8] <- 'Fator CM (fac)'
names(dfProc)[9] <- 'Corrigido (R$)'
names(dfProc)[10] <- 'Corrigido Acum. (R$)'

names(dfProc)[11] <- 'nº JM (meses)'
names(dfProc)[12] <- 'Taxa JM (mês)'
names(dfProc)[13] <- 'Fator JM (fac)'
names(dfProc)[14] <- 'Taxa JM Acum.'
names(dfProc)[15] <- 'JM (R$)'    
names(dfProc)[16] <- 'JM Acum. (R$)'

names(dfProc)[17] <- 'Total (R$)'
names(dfProc)[18] <- 'Total Acum. (R$)'

dfProc$Mês = format(as.Date(dfProc$Mês), '%d/%m/%Y')

#### IMPRESSAO ####
evento3A <- 'MEMÓRIA DE CÁLCULO | Momento 3 | Período 2 | Evento 1'

anexo3A <- 
  # CABEÇALHO
  format(as.data.frame(dfProc), decimal.mark = ',', big.mark = '.') %>%
  kable(align = 'r', caption = evento3A, 'html') %>%
  add_header_above(c(rep('', 18))) %>%
  
  # linha 5
  add_header_above(c(rep('', 4), 
                     'Nº de meses (NM)', nomDataInterv,
                     cmInterv,
                     jrsMorInterv,
                     rep('', 10)), font_size = 10, line = FALSE, align = 'r', bold = FALSE, color = 'gray') %>%
  # linha 4
  add_header_above(c(rep('', 4), 
                     'Final', format(as.Date(nomDataFim), '%d/%m/%Y'), 
                     format(as.Date(cmDataFim), '%d/%m/%Y'),
                     format(as.Date(jrsMorDataFim), '%d/%m/%Y'),
                     rep('', 10)), 
                   font_size = 10, line = FALSE, align = 'r', bold = FALSE, color = 'gray') %>%
  # linha 3
  add_header_above(c(rep('', 4),
                     'Inicial', format(as.Date(nomDataIn), '%d/%m/%Y'), 
                     format(as.Date(cmDataIn), '%d/%m/%Y'),
                     format(as.Date(jrsMorDataIn), '%d/%m/%Y'),
                     rep('', 2),
                     format(as.numeric(tNominal), decimal.mark = ',', big.mark = '.'),
                     format(as.numeric(tReal), decimal.mark = ',', big.mark = '.'),
                     format(as.numeric(tJM), decimal.mark = ',', big.mark = '.'),
                     format(as.numeric(t), decimal.mark = ',', big.mark = '.'),
                     rep('', 4)),
                   font_size = 10, line = FALSE, align = 'r', bold = FALSE, color = 'gray') %>%
  
  # linha 2
  add_header_above(c(rep('', 4),
                     'DATA',
                     'VALOR NOMINAL', 
                     'CM', 
                     'JM',
                     rep('', 2),
                     'NOMINAL', 
                     'CM', 
                     'JM', 
                     'TOTAL',
                     rep('', 4)),
                   font_size = 10, line = FALSE, align = 'r') %>%
  # linha 1
  add_header_above(c(rep('', 4),
                     'TEMPOS' = 4,
                     rep('', 2),
                     'MONTANTES (R$)' = 4,
                     rep('', 4)),
                   font_size = 10, line = TRUE, align = 'c') %>%
  
  # RODAPÉ
  kable_styling(font_size = 10)

anexo3A

#### Fim: MOMENTO 3A @@@A####

#### Tabela MOMENTO 3 ####
options(digits=12)
matrizDf3 <- tibble(Eventos = c('Juros de Mora (Per. 1) (+)', 'Extras (-)'),
                    `Data inicial` = c(cmDataIn3A, ''), 
                    `Data final` = c(cmDataFim3A, ''),
                    `Nominal (R$)` = c(tNominal3A, 0), 
                    `Corrigido (R$) (A)` = c(tReal3A, 0), 
                    `JM (R$) (B)` = c(tJM3A, 0), 
                    `Total (R$) (A+B)` = c(t3A, 0))

matrizDf3$`Data inicial` <- format(as.Date(matrizDf3$`Data inicial`), '%d/%m/%Y')
matrizDf3$`Data final` <- format(as.Date(matrizDf3$`Data final`), '%d/%m/%Y')

matrizDf3 <- matrizDf3 %>%
  adorn_totals('row',  na.rm = TRUE
               ,,,c('Nominal (R$)',
                    'Corrigido (R$) (A)',
                    'JM (R$) (B)',
                    'Total (R$) (A+B)' ))

tab3 <- format(as.data.frame(matrizDf3), decimal.mark = ',', big.mark = '.') %>%
  kable('html', escape = F) %>%
  add_header_above(c('', 'MOMENTO 3 (períodos 1 e 2):\nProcesso judicial, precatório, graça,\npós-graça e momento atual (CF, art. 100, § 5º): \n Datas, valores, correção e juros' = 6)) %>%
  kable_minimal(html_font = 'cambria', font_size = 18)

tab3

#### TABELA - RESULTADO FINAL ####
options(digits=12)
matrizDf4 <- tibble(`Nominal (R$) (A)` = tNominal1, 
                    `Corrigido (R$) (B)` = tReal2, 
                    `Juros de mora (R$) (C)` = sum(tJM1, tJM2, tJM3A), 
                    `Extras (E)` = 0, # ver o que fazer aqui depois, considerando a partir de 3C
                    `Total (R$) (F=B+C+D-E)` =  sum(tReal2, tJM1, tJM2, tJM3A),
                    `Calculado processo (G)` = calcProc,
                    `Diferença (R$) (H=F-G)` = sum(tReal2, tJM1, tJM2, tJM3A) - calcProc)


tab4  <- format(as.data.frame(matrizDf4), decimal.mark = ',', big.mark = '.') %>%
  kable('html', escape = F) %>%
  add_header_above(c('', '', '', 'RESULTADO FINAL: Montante nominal, correção monetária, juros, valor atual no processo e valor a ser acrescentado/retificado', '', '', ''), bold = TRUE) %>%
  kable_minimal(full_width = F, html_font = 'Cambria', font_size = 14)

tab4



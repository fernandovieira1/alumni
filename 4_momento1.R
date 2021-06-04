
#### Inicio: MOMENTO 1 #### @

#### { ####
#### ******************************************************************************** ####
#### 0. CONFIGURAR AMBIENTE ####
#### ******************************************************************************** ####
library(kableExtra)
library(janitor)

#### { ####
#### ******************************************************************************** ####
####  PARTE I: Definir variáveis, valores chave e encontrar o VALOR nominal ####
#### ******************************************************************************** ####

## JUROS
# ** Input form: select (predefinidas) + opção de mudar **

## Mora
jrsMorDataIn <- as.Date(ifelse(jm_criterio==2,
                               dmy(paste(01, 01, year(julgado)+1, sep = '-')),
                               ifelse(jm_criterio==0, 
                                      nomDataIn, 
                                      max(citacao, nomDataIn))), origin ='1970-01-01')

jrsMorDataFim <- min(taxasDataMax,
                     as.Date(ifelse(month(reqDataPrec)>=7&day(reqDataPrec)>1,
                                    dmy(paste(01, 07, year(reqDataPrec)+1, sep = '-')),
                                    dmy(paste(01, 07, year(reqDataPrec), sep = '-'))), origin ='1970-01-01'))

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
#### __ Dados ####
IndPrec <- 'https://docs.google.com/spreadsheets/d/1iYM23jb8MNeZ1inQb2hzrf9L5KrzQPD-_G_Uw5EV7Z0/edit#gid=936438362' # TJ antigo: TR: 12/2009 a 03/2015

#### __ Índices Brutos ####
indicesBrutos <- read_sheet(IndPrec, sheet = bd) # ** Input form: select (predefinidas) **
indicesBrutos$Mes <- as.Date(format(indicesBrutos$Mes, format = '%Y-%m-%d'))
indicesBrutos <- indicesBrutos %>% na.omit()

indicesBrutos <- as.data.frame(indicesBrutos)

indicesBrutos[,3:4] <- round(indicesBrutos[,3:4], 6)
indicesBrutos[,6:7] <- round(indicesBrutos[,6:7], 6)

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
indices$JMMes[which(indices$Mes==jrsMorDataFim)] <- if (day(jrsMorDataFim) == 01 & month(jrsMorDataFim) == 07 & year(jrsMorDataFim) == year(max(indices$Mes))-1) {
  indices$JMMes[which(indices$Mes==jrsMorDataFim)] = (indices$JMMes[which(indices$Mes==jrsMorDataFim)] / 30) * (day(jrsMorDataFim))
} else if (day(jrsMorDataFim) != 1 & day(jrsMorDataFim) != 30 & day(jrsMorDataFim) != 31) {
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
processoJM <- processo %>% filter(JMAcum>0)

for (i in jm) {
  jm <- data.frame(cbind(processoJM$JMAcum[i]), check.names = FALSE)
  jm <- as_tibble(jm) %>% rename(nJRMORFac = 'cbind(processoJM$JMAcum[i])')
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

#### nominal ####
# Ou principal
tNominal <- Totalnominal -> tNominal1

#### Real ####
# Ou corrigido
tReal <- TotalReal # Valor atualizado

##### Juros de Mora (JM) ####
tJM <- processo %>% select(JurosMoraAcum) %>% na.omit() %>% tail(1) %>% as.numeric() %>% round(2)

##### __ Total ####
# Corrigido + Juros
t <- tReal + tJM

#### __ Diferencas ####
# Real - nominal
tDif <- t - tNominal

#### Tabela MOMENTO 1 ####


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
anexo1 <- function(nr_Evento) {
  options(scipen = 12)
  evento <- paste('MEMÓRIA DE CÁLCULO | Momento 1 | Período 1 | evento', nr_Evento)
  # CABEÇALHO
  format(as.data.frame(dfProc), decimal.mark = ',', big.mark = '.') %>%
    kable(align = 'r', caption = evento, 'html') %>%
    add_header_above(c(rep('', 18))) %>%
    
    # linha 5
    add_header_above(c(rep('', 3), 
                       'Nº de meses (NM)', nomDataInterv,
                       cmInterv,
                       jrsMorInterv,
                       rep('', 11)), font_size = 10, line = FALSE, align = 'r', bold = FALSE, color = 'gray') %>%
    # linha 4
    add_header_above(c(rep('', 3), 
                       'Final', format(as.Date(nomDataFim), '%d/%m/%Y'), 
                       format(as.Date(cmDataFim), '%d/%m/%Y'),
                       format(as.Date(jrsMorDataFim), '%d/%m/%Y'),
                       rep('', 11)), 
                     font_size = 10, line = FALSE, align = 'r', bold = FALSE, color = 'gray') %>%
    # linha 3
    add_header_above(c(rep('', 3),
                       'Inicial', format(as.Date(nomDataIn), '%d/%m/%Y'), 
                       format(as.Date(cmDataIn), '%d/%m/%Y'),
                       format(as.Date(jrsMorDataIn), '%d/%m/%Y'),
                       rep('', 5),
                       format(as.numeric(tNominal), decimal.mark = ',', big.mark = '.'),
                       format(as.numeric(tReal), decimal.mark = ',', big.mark = '.'),
                       format(as.numeric(tJM), decimal.mark = ',', big.mark = '.'),
                       format(as.numeric(t), decimal.mark = ',', big.mark = '.'),
                       c(rep('', 2))),
                     font_size = 10, line = FALSE, align = 'r', bold = FALSE, color = 'gray') %>%
    
    # linha 2
    add_header_above(c(rep('', 3),
                       'DATA',
                       'VALOR NOMINAL', 
                       'CM', 
                       'JM', 
                       rep('', 5),
                       'NOMINAL', 
                       'CM', 
                       'JM', 
                       'TOTAL', 
                       c(rep('', 2))),
                     font_size = 10, line = FALSE, align = 'r') %>%
    # linha 1
    add_header_above(c(rep('', 3),
                       'TEMPOS' = 4,
                       rep('', 5),
                       'MONTANTES (R$)' = 4,
                       c(rep('', 2))),
                     font_size = 10, line = TRUE, align = 'c') %>%
    
    # RODAPÉ
    kable_styling(font_size = 10)
}
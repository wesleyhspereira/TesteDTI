##########################################################################################
# Teste pr�tico DTI Digital                                                              #
# Por Wesley Henrique Silva Pereira                                                      #  
##########################################################################################

rm(list = ls()) # Limpando mem�ria

################## Leitura e manipula��o pr�via do banco de dados #######################

# Mudando para o diret�rio do arquivo Script.R (funciona se estiver usando RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Lendo banco de dados (na.strings para substituir "?" por Not Avaliable)
dados = read.csv2("wiki4HE.csv", na.strings = "?")

# Dimens�es do banco de dados

dim(dados)

# Listando vari�veis num�ricas

nmc = c("AGE","YEARSEXP")

# Listando vari�veis categ�ricas

fct = colnames(dados)[which(!colnames(dados) %in% nmc)]

# Verificando classe das colunas do banco de dados

str(dados)

### Alterando as colunas que categ�ricas para a classe adequada

# Para instalar bibliotecas ori�ndas de reposit�rios remotos com "GitHub" 
if(!require(remotes)){install.packages("remotes");require(remotes)} 

# Biblioteca hfunk do reposit�rio hstojic

if(!require(hfunk)){remotes::install_github("hstojic/hfunk");require(hfunk)} 

# Mudando classe das colunas do banco de dados

dados = changeClass(dados, 
            ifelse(1:dim(dados)[2] %in% which(!colnames(dados) %in% nmc),"fac","num"))

str(dados)

###################### Calculando estat�sticas descritivas #############################

# Biblioteca psych

if(!require(psych)){remotes::install_github("psych");require(psych)} 

# Descrevendo vari�veis num�ricas (fun��o describe da biblioteca psych)

descnmc = psych::describe(dados[,nmc])

# Moda idade

as.numeric(names(table(dados$AGE))[which(table(dados$AGE) == max(table(dados$AGE)))])

# Moda experiencia

as.numeric(names(table(
  dados$YEARSEXP))[which(table(dados$YEARSEXP) == max(table(dados$YEARSEXP)))])

# Descrevendo vari�veis categ�ricas (fun��o describe da biblioteca Hmisc)

descfct = apply(dados[,fct],2,
                function(x)cbind('Frequencia absoluta' = table(x,useNA = "ifany"),
                'Frequencia relativa' = ifelse(sum(is.na(x)) & table(x,useNA = "ifany"),
                 c(prop.table(table(x)),NA),
                prop.table(table(x)))))


################################## An�lise ENJ1 e ENJ2 ###########################################

if(!require(Kendall)){install.packages("Kendall");require("Kendall")}

############ Faixa et�ria

# Calculando amplitude da classe

ceiling((max(na.omit(dados$YEARSEXP)) - min(na.omit(dados$YEARSEXP)))/5)

# Dividindo as idades em faixas et�rias

AGEC = ifelse(dados$AGE < 33,1,ifelse(dados$AGE < 43, 2,
              ifelse(dados$AGE < 53,3,ifelse(dados$AGE < 63,4,5))))

# Tabela cruzada

table(AGEC,dados$ENJ1)

# Teste de Kendall para associa��o

Kendall(AGEC,dados$ENJ1)

# Tabela cruzada

table(AGEC,dados$ENJ2)

# Teste de Kendall para associa��o

Kendall(AGEC,dados$ENJ2)

############ Faixa de anos de experi�ncia

# Calculando amplitude da classe

ceiling((max(na.omit(dados$YEARSEXP)) - min(na.omit(dados$YEARSEXP)))/4)

# Dividindo as idades em faixa de anos de experi�ncia

YEXC = ifelse(dados$YEARSEXP < 9,1,ifelse(dados$YEARSEXP < 18, 2,
              ifelse(dados$YEARSEXP < 27, 3, ifelse(dados$YEARSEXP < 36, 4, 5))))
# Tabela cruzada

table(YEXC,dados$ENJ1)

# Teste de Kendall para associa��o

Kendall(YEXC,dados$ENJ1)

# Tabela cruzada

table(YEXC,dados$ENJ2)

# Teste de Kendall para associa��o

Kendall(YEXC,dados$ENJ2)

# Teste de Friedman

# UOC_POSITION vs ENJ1

friedman.test(as.matrix(table(dados$UOC_POSITION,dados$ENJ1)))

# UOC_POSITION vs ENJ1

friedman.test(as.matrix(table(dados$UOC_POSITION,dados$ENJ2)))

# DOMAIN vs ENJ1

friedman.test(as.matrix(table(dados$DOMAIN,dados$ENJ1)))

# DOMAIN vs ENJ1

friedman.test(as.matrix(table(dados$DOMAIN,dados$ENJ2)))

# Regress�o log�stica

# Ajustando modelo

mod = glm(USERWIKI ~ -1 + GENDER + Use1 + Exp5, family = binomial, data = dados)

# Resumo do modelo ajustado

summary(mod)

# Predi��o via modelo

modfit = fitted(mod)

# Criando curva ROC

p = seq(0,1,0.001)
sen = esp =  rep(0,length(p))
real = as.numeric(as.character(dados$USERWIKI[1:913 %in% as.numeric(names(modfit))]))
for(i in 1:length(p)){
  fits = ifelse(modfit < p[i],0,1)
  esp[i] = sum(real == 0 & fits == 0)/sum(real == 0)
  sen[i] = sum(real == 1 & fits == 1)/sum(real == 1)
}

# Curva ROC

plot(1-esp,sen,type = "l")

# Encontrando limiar

plot(p,sen,type = "l")
lines(p,esp)

# Testando os valores preditos

pred = function(v){exp(coef[1,1] + v%*%coef[-1,1])/(1+exp(coef[1,1] + v%*%coef[-1,1]))}

pred(c(1,
       0,1,0,0,
       1,0,0,0))

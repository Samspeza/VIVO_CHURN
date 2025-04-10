R.version.string
#Pacotes necessarios - instalação
install.packages('tidyverse') #manipulação de dados
install.packages('ggplot2') #visualização
install.packages('cowplot') #visualização - unir gráficos
install.packages('caret') #modelos estatísticos
install.packages('corrplot') #matriz de correlação
#chamndo os pacotes ja instalados
library(caret)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(corrplot)

#carregando dados para o Data Frame (dados)
dados <-read.csv("VIVO_CHURN.csv", stringsAsFactors = T)
#Visualização dos dados
glimpse(dados)
summary(dados)
#Check dados faltantes e retirar
colSums(is.na(dados)) #se nulo
dados_1 <- dados[!is.na(dados$TotalCharges),]
 #diferente de nulo para o novo data frame (dados_1)
colSums(is.na(dados_1))
glimpse(dados_1)

#outliers - visualizar dispersão dos dados via boxplot
dados_1 %>%
ggplot(aes(x=Churn,y=tenure, fill=Churn)) +
geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)
dados_1 %>%
ggplot(aes(x=Churn,y=MonthlyCharges, fill=Churn)) +
geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)
dados_1 %>%
ggplot(aes(x=Churn,y=TotalCharges, fill=Churn)) +
geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)

#verificando a classificação de cada variável - categórica(fatores)
ggplot(dados, aes(y = gender, x = gender, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = Partner, x = Partner, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = Dependents, x = Dependents, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = PhoneService, x = PhoneService, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = MultipleLines, x = MultipleLines, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = InternetService, x = InternetService, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = OnlineSecurity, x = OnlineSecurity, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = OnlineBackup, x = OnlineBackup, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = DeviceProtection, x = DeviceProtection, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = TechSupport, x = TechSupport, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = StreamingTV, x = StreamingTV, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = StreamingMovies, x = StreamingMovies, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = Contract, x = Contract, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = PaperlessBilling, x = PaperlessBilling, fill = Churn)) + geom_bar(stat = "identity")
ggplot(dados, aes(y = PaymentMethod, x = PaymentMethod, fill = Churn)) + geom_bar(stat = "identity")

#verificando a classificação de cada variável - numérica
ggplot(dados,
aes(x = MonthlyCharges,
fill = Churn)) +
geom_density(alpha = 0.4) +
labs(title = "")
ggplot(dados,
aes(x = TotalCharges,
fill = Churn)) +
geom_density(alpha = 0.4) +
labs(title = "")
ggplot(dados,
aes(x = SeniorCitizen,
fill = Churn)) +
geom_density(alpha = 0.4) +
labs(title = "")

#transformação de dados (qualitativo para quantitativo) - variáveis binárias
dados_quant <- dados_1
colnames (dados_quant)
dados_quant %>%
mutate(customerID = NULL,
PhoneService = as.factor(ifelse(PhoneService == "Yes",1,0)),
Partner = as.factor(ifelse(Partner == "Yes",1,0)),
Dependents = as.factor(ifelse(Dependents == "Yes",1,0)),
PaperlessBilling = as.factor(ifelse(TechSupport == "Yes",1,0)),
Dependents = as.factor(ifelse(Dependents == "Yes",1,0)),
Churn = as.factor(ifelse(Churn == "Yes",1,0)),
) -> dados_quant
glimpse(dados_quant)

#utilização de variáveis dummy
dummy_dados <- dados_quant %>% select(InternetService, Contract, PaymentMethod,
MultipleLines, OnlineBackup, OnlineSecurity,
DeviceProtection, StreamingTV,
StreamingMovies, TechSupport)
dummy <- dummyVars(~ ., data = dummy_dados, fullRank = T)
dummy_dados <- predict(dummy, dummy_dados)
dados_quant1 <- bind_cols(dados_quant,dummy_dados)
dados_quant1 %>%
rename( InternetService.Fiberoptic =`InternetService.Fiber optic`,
Contract.Oneyear = `Contract.One year`,
Contract.Twoyear = `Contract.Two year`,
PaymentMethod.Creditcard = `PaymentMethod.Credit card (automatic)`,
PaymentMethod.Electronic = `PaymentMethod.Electronic check`,
PaymentMethod.Mailed = `PaymentMethod.Mailed check`,
MultipleLines.NoService = `MultipleLines.No phone service`,
OnlineBackup.NoService = `OnlineBackup.No internet service` ,
OnlineSecurity.NoService = `OnlineSecurity.No internet service`,
DeviceProtection.NoService = `DeviceProtection.No internet service`,
StreamingTV.NoService = `StreamingTV.No internet service`,
StreamingMovies.NoService = `StreamingMovies.No internet service`,
TechSupport.NoService = `TechSupport.No internet service`) -> dados_quant1

#exclusao de variaveis qualitativas
dados_quant1 %>%
mutate(gender = NULL,
InternetService = NULL,
Contract = NULL,
PaymentMethod = NULL,
PaymentMOnlineBackupethod = NULL,
OnlineSecurity = NULL,
StreamingTV = NULL,
StreamingMovies = NULL,
MultipleLines = NULL,
OnlineBackup = NULL,
DeviceProtection = NULL,
TechSupport = NULL) -> dados_quant1
glimpse(dados_quant1)

#Correlação dos dados
# todas que estão com <fct> transformar para numérico
dados_quant1 %>%
mutate( Partner = as.numeric(Partner),
Dependents = as.numeric(Dependents),
PhoneService = as.numeric(PhoneService),
PaperlessBilling = as.numeric(PaperlessBilling),
Churn = as.numeric(Churn)) -> dados_num
glimpse(dados_num)
corrplot(cor(dados_num), method = "circle")
dados_num %>%
mutate( Dependents = NULL) -> dados_num_1
#glimpse(dados_num_1)
corrplot(cor(dados_num_1),order = "hclust", method = "circle")
dados_num_1 %>%
mutate(StreamingMovies.NoService = NULL,
StreamingTV.NoService = NULL,
DeviceProtection.NoService = NULL ,
OnlineSecurity.NoService = NULL,
InternetService.No = NULL,
OnlineBackup.NoService = NULL ) -> dados_num_1

#balanceamento de base de dados
dados_quant1 %>%
select(Churn) %>%
group_by(Churn) %>%
summarise(n = n())
dados_quant1 %>%
filter(Churn == 0) %>%
sample_n(1869) -> dados_quant_0
dados_quant1 %>%
filter(Churn == 1) -> dados_quant_1
dados_quant_balanc <- bind_rows(dados_quant_0,dados_quant_1)
dados_num_1 %>%
select(Churn) %>%
group_by(Churn) %>%
summarise(n = n())
dados_num_1 %>%
mutate(Churn = ifelse(Churn == 1,0,1))%>%
filter(Churn == 0) %>%
sample_n(1869) -> dados_num_0
dados_num_1 %>%
mutate(Churn = ifelse(Churn == 1,0,1))%>%
filter(Churn == 1) -> dados_num_2
dados_num_balanc <- bind_rows(dados_num_2,dados_num_0)
dados_num_balanc %>%
select(Churn) %>%
group_by(Churn) %>%
summarise(n = n())
dados_quant_balanc %>%
select(Churn) %>%
group_by(Churn) %>%
summarise(n = n())

#stepwize
#verificar quais as variaveis agregam ao modelo
modelo_teste1 <- glm(data = dados_num_balanc, Churn ~ .,family=binomial)
step(modelo_teste1)
summary(modelo_teste1)

# Validação cruzada - avalia a capacidade de generalização do modelo
#O método de validação cruzada denominado k-fold consiste em dividir o conjunto
#total de dados em k subconjuntos mutuamente exclusivos do mesmo tamanho e,
#a partir daí, um subconjunto é utilizado para teste e os k-1 restantes são
#utilizados para estimação dos parâmetros, fazendo-se o cálculo da acurácia do modelo.
#Este processo é realizado k vezes alternando de forma circular o subconjunto de teste.
trainIndex <- createDataPartition(dados_num_balanc$Churn, p = .8, #80 por cento
list = FALSE,
times = 1)
vivoTrain <- dados_num_balanc[ trainIndex,]
vivoTest <- dados_num_balanc[-trainIndex,]

#treinando a base
set.seed(150)
glm_model = train(Churn ~ SeniorCitizen + tenure + PaperlessBilling +
MonthlyCharges + TotalCharges + InternetService.Fiberoptic +
Contract.Contract.Oneyear + Contract.Twoyear + PaymentMethod.Electronic +
MultipleLines.Yes + OnlineSecurity.Yes + StreamingTV.Yes +
StreamingMovies.Yes + TechSupport.NoService,
data= vivoTrain,
method= "glm",
trControl = trainControl(method = "cv"),
family = "binomial")
summary(glm_model)
varImp(glm_model)

#testando a base
reg_log_pred <- predict(glm_model,vivoTrain)
reg_log_pred1 <- data.frame(reg_log_pred)
reg_log_pred1$reg_log_pred <- as.factor(ifelse(reg_log_pred1$reg_log_pred >= 0.5,1,0))
reg_log_pred1$reg_log_pred <- as.factor(ifelse(reg_log_pred1$reg_log_pred == 1,"evadido","cliente"))
vivoTrain$Churn <- as.factor(vivoTrain$Churn)
vivoTrain$Churn <- as.factor(ifelse(vivoTrain$Churn == 1,"evadido","cliente"))
glimpse(reg_log_pred1)
matrix_reg <-
confusionMatrix(data = reg_log_pred1$reg_log_pred, reference = vivoTrain$Churn, positive = "evadido")
matrix_reg$table
metricas <- data.frame(matrix_reg$byClass)

glimpse(dados_1)
#excluir o ID
#transformar em qualitativa as quantitativas
#Adicionar classes as variáveis numericas
dados_1 %>%
mutate (SeniorCitizen = as.factor(ifelse(SeniorCitizen == 1, "Yes", "No")),
customerID = NULL) -> dados_quali

#criando classes para as variaveis quantitativas
tenure <- summary(dados_quali$tenure)
tenure

TotalCharges <- summary(dados_quali$TotalCharges)
min_TotalCharges <- TotalCharges[[1]]-5
q1_TotalCharges <- TotalCharges[[2]]
q2_TotalCharges <- TotalCharges[[3]]
q3_TotalCharges <- TotalCharges[[5]]
max_TotalCharges <- TotalCharges[[6]]+5
dados_quali %>%
mutate(TotalCharges = cut(TotalCharges, breaks = c(min_TotalCharges,
q1_TotalCharges,
q2_TotalCharges,
q3_TotalCharges,
max_TotalCharges))) -> dados_quali
summary(dados_quali$TotalCharges)

#balanceamento da base
glimpse(dados_quali)
dados_quali %>%
select(Churn) %>%
group_by(Churn) %>%
summarise(n = n())
dados_quali %>%
filter(Churn == "No") %>%
sample_n(1869) -> dados_quali_no
dados_quali %>%
filter(Churn == "Yes") -> dados_quali_yes
dados_quali_balanc <- bind_rows(dados_quali_no,dados_quali_yes)
dados_quali_balanc %>%
select(Churn) %>%
group_by(Churn) %>%
summarise(n = n())

# #separação da base para teste e treino
trainIndex_quali <- createDataPartition(dados_quali_balanc$Churn, p = .8,
list = FALSE,
times = 1)
vivoTrain_quali <- dados_quali_balanc[ trainIndex_quali,]
vivoTest_quali <- dados_quali_balanc[-trainIndex_quali,]

#treinando o modelo de árvore de decisão
vivo.tree = train(Churn ~ .,
data= vivoTrain_quali,
method="rpart",
trControl = trainControl(method = "cv"))
vivo.tree
fancyRpartPlot(vivo.tree$finalModel)
vivo.pred = predict(vivo.tree, newdata = vivoTrain_quali)
matrix_tree <- confusionMatrix(data = vivo.pred, reference = vivoTrain_quali$Churn, positive = "Yes")
matrix_tree$table
metricas_tree <- data.frame(matrix_tree$byClass)
metricas_tree
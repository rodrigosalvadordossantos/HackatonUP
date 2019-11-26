#Importar o csv
estacao1=read.csv("PRSA_Data_Aotizhongxin_20130301-20170228.csv")
estacao2=read.csv("PRSA_Data_Changping_20130301-20170228.csv")
estacao3=read.csv("PRSA_Data_Dingling_20130301-20170228.csv")
estacao4=read.csv("PRSA_Data_Dongsi_20130301-20170228.csv")
estacao5=read.csv("PRSA_Data_Guanyuan_20130301-20170228.csv")
estacao6=read.csv("PRSA_Data_Gucheng_20130301-20170228.csv")
estacao7=read.csv("PRSA_Data_Huairou_20130301-20170228.csv")
estacao8=read.csv("PRSA_Data_Nongzhanguan_20130301-20170228.csv")
estacao9=read.csv("PRSA_Data_Shunyi_20130301-20170228.csv")
estacao10=read.csv("PRSA_Data_Tiantan_20130301-20170228.csv")
estacao11=read.csv("PRSA_Data_Wanliu_20130301-20170228.csv")
estacao12=read.csv("PRSA_Data_Wanshouxigong_20130301-20170228.csv")

estacoes = list(estacao1,estacao2,estacao3,estacao4,estacao5,estacao6,estacao7,estacao8,estacao9,estacao10,estacao11,estacao12)

#Transformar a coluna WD em ID
wd = c('E','N','W','S','NE','NW','SE','SW','NNE','ENE','ESE','SSE','SSW','WSW','WNW','NNW')
id = c(1:16)
wd_id = data.frame(wd=wd, id=id)

mudarWDemID = function(wdcol){
	retorno = data.frame()
	for(i in 1:length(wdcol)){
		retorno = rbind(retorno, wd_id[wd == wdcol[i],]$id)
		if (is.na(retorno[i,])){
			retorno[i,]=0
		}
	}
	return(retorno)
}

mudarTodosWD = function(estacoes){
	for (i in 1:12){
		wd=mudarWDemID(estacoes[[i]]$wd)
		names(wd)="wd"
		estacoes[[i]]$wd = wd$wd
		print(c("estacao",i,"concluida"))
	}
	return(estacoes)
}

estacoes = mudarTodosWD(estacoes)

#Imputar a media em todos os NA
contarNA = function(df){
	somaNA=0
	for(i in 1:ncol(df)){
		for(j in 1:nrow(df)){
			if (is.na(df[j,i])){
				somaNA = somaNA + 1
			}
		}
	}
	return (somaNA)
}

imputarMedia = function(df){
	for(i in 1:ncol(df)){
		media = mean(na.omit(df[,i]))
		for(j in 1:nrow(df)){
			if (is.na(df[j,i])){
				df[j,i]=media
			}
		}
	}
	return (df)
}

checarMedias = function(df){
	mediaNA = data.frame()
	media = data.frame()
	for(i in 1:ncol(df)){
		mediaNA = rbind(mediaNA,mean(df[,i]))
		media = rbind(media,mean(na.omit(df[,i])))
	}
	retorno = data.frame(mNA=mediaNA,m=media)
	return(retorno)
}

#Verificar se a imputação eliminou os NAs
imputarTodasMedias = function(estacoes){
	for (i in 1:12){
		print(c("NAs em",i,"antes: ",contarNA(estacoes[[i]])))
		estacoes[[i]] = imputarMedia(estacoes[[i]])
		print(c("NAs em",i,"depois: ",contarNA(estacoes[[i]])))
	}
	return (estacoes)
}

estacoes = imputarTodasMedias(estacoes)

#Estatísticas do conjunto
#Graficos e tabelas de correlação para PM2.5
#plot(estacoes[[1]][c(6,8,9,10,11,12,13,14,15,16,17)])
cor(estacoes[[1]][c(6,8,9,10,11,12,13,14,15,16,17)])
#Graficos e tabelas de correlação para PM10
#plot(estacoes[[1]][c(7,8,9,10,11,12,13,14,15,16,17)])
cor(estacoes[[1]][c(7,8,9,10,11,12,13,14,15,16,17)])

#Verificar multicolineariade do PM2.5
verificarMulticolinearidades = function(estacoes){
	for (i in 1:12){
		pm25 <- lm(PM2.5 ~ SO2 + NO2 + CO + O3 + TEMP + PRES + DEWP + RAIN + wd + WSPM, data = estacoes[[i]])
		print(1/(1-summary(pm25)$r.squared))
	}

	for (i in 1:12){
		pm10 <- lm(PM10 ~ SO2 + NO2 + CO + O3 + TEMP + PRES + DEWP + RAIN + wd + WSPM, data = estacoes[[i]])
		print(1/(1-summary(pm10)$r.squared))
	}
	return (estacoes)
}

estacoes = verificarMulticolinearidades(estacoes)

#Construir o modelo que mais se ajuste aos dados
#install.packages("MASS")
library(MASS)

for (i in 1:12){
	#Modelo para PM2.5
	modelo.inicial = lm(PM2.5 ~ SO2 + NO2 + CO + O3 + TEMP + PRES + DEWP + RAIN + wd + WSPM, data = estacoes[[i]])
	modelo.simples = lm(PM2.5 ~ SO2, data = estacoes[[i]])
	modelo.escolhido = stepAIC(modelo.inicial, scope = list(upper = modelo.inicial, lower = modelo.simples), direction = "backward")
	#stepAIC(modelo.simples, scope = list(upper = modelo.inicial, lower = modelo.simples), direction = "forward")
	
	#Modelo escolhido para PM2.5
	modeloPM2.5 = modelo.escolhido
	#plot(modeloPM2.5)
	summary(modeloPM2.5)
	
	#Modelo para PM10
	modelo.inicial = lm(PM10 ~ SO2 + NO2 + CO + O3 + TEMP + PRES + DEWP + RAIN + wd + WSPM, data = estacoes[[i]])
	modelo.simples = lm(PM10 ~ SO2, data = estacoes[[i]])
	modelo.escolhido = stepAIC(modelo.inicial, scope = list(upper = modelo.inicial, lower = modelo.simples), direction = "backward")
	#stepAIC(modelo.simples, scope = list(upper = modelo.inicial, lower = modelo.simples), direction = "forward")
	
	#Modelo escolhido para PM10
	modeloPM10 = modelo.escolhido
	#plot(modeloPM10)
	summary(modeloPM10)

	#Intervalos de confiança dos coeficientes dos modelos
	confint(modeloPM2.5)
	confint(modeloPM10)
}
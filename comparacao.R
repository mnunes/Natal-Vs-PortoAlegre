library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(reshape2)

# site: http://www.inmet.gov.br/projetos/rede/pesquisa/
# precisa de cadastro, mas eh gratuito e automatico

# leitura dos dados - 5 anos

natal      <- as_data_frame(read.table(file="natal.csv", sep=";", header=TRUE))
natal$Data <- dmy(natal$Data)

natal.media <- natal %>%
  select(Data, Temp.Comp.Media) %>%
  na.omit

poa <- as_data_frame(read.table(file="portoalegre.csv", sep=";", header=TRUE))

poa$Data <- dmy(poa$Data)

poa.media <- poa %>%
  select(Data, Temp.Comp.Media) %>%
  na.omit

media <- inner_join(poa.media, natal.media, by="Data")

names(media) <- c("Data", "Porto Alegre", "Natal")

media <- data_frame(Data=c(media$Data, media$Data), 
                    Cidade=rep(c("Porto Alegre", "Natal"), each=length(media$Data)), 
                    Temperatura=c(media$"Porto Alegre", media$Natal))

ggplot(media, aes(x=Data, y=Temperatura, colour=Cidade)) +
  geom_line() +
  #geom_smooth(method="lm", se=FALSE) +
  scale_x_date(breaks=seq(min(media$Data), max(media$Data), by="6 month"), date_labels="%b/%Y", minor_breaks=seq(min(media$Data), max(media$Data), by="6 month")) +
  scale_y_continuous(breaks=seq(0, 35, 5))



# leitura dos dados - completo

natal      <- as_data_frame(read.table(file="natal_completo.csv", sep=";", header=TRUE))
natal$Data <- dmy(natal$Data)

natal.media <- natal %>%
  select(Data, Temp.Comp.Media) %>%
  na.omit

poa <- as_data_frame(read.table(file="portoalegre_completo.csv", sep=";", header=TRUE))

poa$Data <- dmy(poa$Data)

poa.media <- poa %>%
  select(Data, Temp.Comp.Media) %>%
  na.omit

media <- inner_join(poa.media, natal.media, by="Data")

names(media) <- c("Data", "Porto Alegre", "Natal")

media <- data_frame(Data=c(media$Data, media$Data), 
                    Cidade=rep(c("Porto Alegre", "Natal"), each=length(media$Data)), 
                    Temperatura=c(media$"Porto Alegre", media$Natal))

ggplot(media, aes(x=Data, y=Temperatura, colour=Cidade)) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  scale_x_date(breaks=seq(min(media$Data), max(media$Data), by="5 year"), date_labels="%Y", minor_breaks=seq(min(media$Data), max(media$Data), by="5 year")) +
  scale_y_continuous(breaks=seq(0, 35, 5))



# leitura dos dados - 15 anos

natal      <- as_data_frame(read.table(file="natal_completo.csv", sep=";", header=TRUE))
natal$Data <- dmy(natal$Data)

natal.media <- natal %>%
  select(Data, Temp.Comp.Media) %>%
  filter(Data>="2002-01-01") %>%
  na.omit


poa <- as_data_frame(read.table(file="portoalegre_completo.csv", sep=";", header=TRUE))

poa$Data <- dmy(poa$Data)

poa.media <- poa %>%
  select(Data, Temp.Comp.Media) %>%
  filter(Data>="2002-01-01") %>%
  na.omit

media <- inner_join(poa.media, natal.media, by="Data")

names(media) <- c("Data", "Porto Alegre", "Natal")

media <- data_frame(Data=c(media$Data, media$Data), 
                    Cidade=rep(c("Porto Alegre", "Natal"), each=length(media$Data)), 
                    Temperatura=c(media$"Porto Alegre", media$Natal))

ggplot(media, aes(x=Data, y=Temperatura, colour=Cidade)) +
  geom_line() +
  #geom_smooth(method="lm", se=FALSE) +
  scale_x_date(breaks=seq(min(media$Data), max(media$Data), by="1 year"), date_labels="%Y", minor_breaks=seq(min(media$Data), max(media$Data), by="1 year")) +
  scale_y_continuous(breaks=seq(0, 35, 5))



# leitura dos dados - 15 anos - maxima

natal      <- as_data_frame(read.table(file="natal_completo.csv", sep=";", header=TRUE))
natal$Data <- dmy(natal$Data)

natal.media <- natal %>%
  select(Data, TempMaxima) %>%
  filter(Data>="2002-01-01") %>%
  na.omit


poa <- as_data_frame(read.table(file="portoalegre_completo.csv", sep=";", header=TRUE))

poa$Data <- dmy(poa$Data)

poa.media <- poa %>%
  select(Data, TempMaxima) %>%
  filter(Data>="2002-01-01") %>%
  na.omit

media <- inner_join(poa.media, natal.media, by="Data")

names(media) <- c("Data", "Porto Alegre", "Natal")

media <- data_frame(Data=c(media$Data, media$Data), 
                    Cidade=rep(c("Porto Alegre", "Natal"), each=length(media$Data)), 
                    Temperatura=c(media$"Porto Alegre", media$Natal))

ggplot(media, aes(x=Data, y=Temperatura, colour=Cidade)) +
  geom_line(alpha=.5) +
  scale_x_date(breaks=seq(min(media$Data), max(media$Data), by="1 year"), date_labels="%Y", minor_breaks=seq(min(media$Data), max(media$Data), by="1 year")) +
  labs(title="Temperatura Máxima Diária") +
  scale_y_continuous(breaks=seq(0, 40, 5)) +
  theme(plot.title = element_text(hjust = 0.5))

sum((filter(media, Cidade == "Porto Alegre")$Temperatura > filter(media, Cidade == "Natal")$Temperatura)) / length(filter(media, Cidade == "Natal")$Temperatura)

sum((filter(media, Cidade == "Natal")$Temperatura > filter(media, Cidade == "Porto Alegre")$Temperatura)) / length(filter(media, Cidade == "Natal")$Temperatura)

sum((filter(media, Cidade == "Natal")$Temperatura == filter(media, Cidade == "Porto Alegre")$Temperatura)) / length(filter(media, Cidade == "Natal")$Temperatura)




# amplitudes comparadas

# leitura dos dados - 15 anos - maxima

# natal

natal      <- as_data_frame(read.table(file="natal_completo.csv", sep=";", header=TRUE))
natal$Data <- dmy(natal$Data)

natal.minima <- natal %>%
  select(Data, TempMinima) %>%
  filter(Data>="2002-01-01") %>%
  na.omit

natal.media <- natal %>%
  select(Data, Temp.Comp.Media) %>%
  filter(Data>="2002-01-01") %>%
  na.omit

natal.maxima <- natal %>%
  select(Data, TempMaxima) %>%
  filter(Data>="2002-01-01") %>%
  na.omit

natal <- inner_join(inner_join(natal.minima, natal.media, by="Data") , natal.maxima, by="Data")

# porto alegre

poa      <- as_data_frame(read.table(file="portoalegre_completo.csv", sep=";", header=TRUE))
poa$Data <- dmy(poa$Data)

poa.minima <- poa %>%
  select(Data, TempMinima) %>%
  filter(Data>="2002-01-01") %>%
  na.omit

poa.media <- poa %>%
  select(Data, Temp.Comp.Media) %>%
  filter(Data>="2002-01-01") %>%
  na.omit

poa.maxima <- poa %>%
  select(Data, TempMaxima) %>%
  filter(Data>="2002-01-01") %>%
  na.omit

poa <- inner_join(inner_join(poa.minima, poa.media, by="Data") , poa.maxima, by="Data")

# juntar os dados

dados <- inner_join(natal, poa, by="Data")

names(dados) <- c("Data", "MinimaNatal", "MediaNatal", "MaximaNatal", "MinimaPoa", "MediaPoa", "MaximaPoa")

ggplot(dados, aes(x=Data, y=MinimaNatal)) +
  geom_smooth(aes(y=MinimaNatal, colour="Natal"), se=FALSE, method="loess", span=0.25) +
  geom_smooth(aes(y=MediaNatal, colour="Natal"), se=FALSE, method="loess", span=0.25) +
  geom_smooth(aes(y=MaximaNatal, colour="Natal"), se=FALSE, method="loess", span=0.25) +
  geom_smooth(aes(y=MinimaPoa, colour="Porto Alegre"), se=FALSE, method="loess", span=0.25) +
  geom_smooth(aes(y=MediaPoa, colour="Porto Alegre"), se=FALSE, method="loess", span=0.25) +
  geom_smooth(aes(y=MaximaPoa, colour="Porto Alegre"), se=FALSE, method="loess", span=0.25) +
  scale_x_date(breaks=seq(min(media$Data), max(media$Data), by="1 year"), date_labels="%Y", minor_breaks=seq(min(media$Data), max(media$Data), by="1 year")) +
  scale_y_continuous(breaks=seq(0, 40, 5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Temperatura")



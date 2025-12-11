
# Paper: "Partisan and Nonpartisan Models of Constitution-Making: A Comparative Case Study of Brazil and Chile"
# Authors: Rodolfo de Camargo Lima e Danilo Medeiros
# Journal of Politics in Latin America
# https://doi.org/10.1177/1866802X251397954

# Script to replicate figures 1-7

#Define the working directory
setwd("C:\\Users\\yourname\\yourdirectory\\") #change to your own

#Load packages
library(tidyverse)
library(tidylog)
library(readxl)
library(wnominate)
library(emIRT)
library(ggthemes)

# Roll Calls in Chile's floor
vot <- read_excel("Votaciones_CC_.xlsx", 
                                       sheet = "Sesion Plenaria")

vot <- vot %>%
  pivot_longer(cols = "Núñez, Nicolás":"Tepper, María Angélica",
                    names_to = "apellido_nombre",
                    values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)

# Change variable vote
vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                        ifelse(vot$voto %in% c("En Contra"), -1, 
                               ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto
  )

# Bring in data from legislators
perfil <- read_excel("perfiles_id_clean_novo.xlsx")
perfil$id <- perfil$id...1
perfil$id...1 <- NULL
perfil$id...3 <- NULL

# Merge votes and legislators info
partido <- read_excel("perfiles_id_clean_novo.xlsx", sheet = "partidos_indep")

partido$`...2` <- NULL
partido$`...3`<- NULL
partido$`...4`<- NULL
partido$`...5`<- NULL
partido$`...6`<- NULL

partido$`tipo partido o independ` <- NULL

partido <- unique(partido)

perfil <- left_join(perfil, partido)

pacto <- read_excel("perfiles_id_clean_novo.xlsx", sheet = "pacto")

perfil <- left_join(perfil, pacto)

#

nominais <- left_join(perfil,vot)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

# Create object to run the wnomante

r <- rollcall(nominais[,15:length(nominais)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[,1:14])

# wnominate - Chile
wnom <- wnominate(r, dims=1, minvotes=20, lop=0.025,polarity=c(38), verbose=FALSE)

windows()
plot(wnom)
plot.coords(wnom)

legis <- wnom$legislators
legis <- na.omit(legis)

basic <- legis %>% 
  summarize(
    mean=mean(coord1D, na.rm=T),
    min=min(coord1D, na.rm=T), 
    max=max(coord1D, na.rm=T), 
    SD=sd(coord1D, na.rm=T))

# Figure 4 - Ideal points of Chilean CC members by parties (W-Nominate scores) 
legis$party <- with(legis, reorder(party, coord1D, mean))

f4 <- ggplot(legis, aes(coord1D, party)) + geom_jitter() +
  xlab("") + ylab("") +
  #ggtitle("Figure 4 - Ideal points of Chilean CC members by parties (W-Nominate scores) ") +
  geom_vline(xintercept = -0.3548171, linetype=2) +
  geom_vline(xintercept = 0, linetype=1) +
  #theme_economist() + scale_colour_economist()
  theme_bw()

#tiff("figure4.tiff", units="in", width=6, height=4, res=900)
#f4 <- gridExtra::grid.arrange(f4, bottom="Figure 4 - Ideal points of Chilean CC members by parties (W-Nominate scores)")
#dev.off()

png("figure4.png", units="in", width=6, height=4, res=900)
f4
dev.off()

#Figure 6 - Ideal points of Chilean CC members by coalitions (W-Nominate scores)

legis$nome_pactos <- with(legis, reorder(nome_pactos, coord1D, mean))

f6 <- ggplot(legis, aes(coord1D, nome_pactos)) + geom_jitter() +
  xlab("") + ylab("") +
  #ggtitle("Ideal Points - CC Chile") +
  geom_vline(xintercept = -0.3548171, linetype=2) +
  geom_vline(xintercept = 0, linetype=1) +
  theme_bw()

#tiff("figure6.tiff", units="in", width=6, height=4, res=900)
#f6 <- gridExtra::grid.arrange(f6, bottom="Figure 6 - Ideal points of Chilean CC members by coalitions (W-Nominate scores)")
#dev.off()

png("figure6.png", units="in", width=6, height=4, res=900)
f6
dev.off()

# Figure 7 - Ideal points of Chilean CC independent members by coalitions (W-Nominate scores)
indep <- filter(legis, party == "INDEPENDIENTE")

f7 <- ggplot(indep, aes(coord1D, nome_pactos, shape = as.character(inp_ip_mp))) + geom_jitter() +
  xlab("") + ylab("") +
  #ggtitle("Ideal Points - CC Chile") +
  geom_vline(xintercept = -0.4377342, linetype=2) +
  geom_vline(xintercept = 0, linetype=1) +
  scale_shape_discrete(name  ="Type of (non)partisanship",
                       breaks=c(0,1,2),
                       labels=c("Nonpartisan Indep.","Partisan Indep.","Party Militants")) +
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0))

#tiff("figure7.tiff", units="in", width=7, height=4, res=900)
#f7 <- gridExtra::grid.arrange(f7, bottom="Figure 7 - Ideal points of Chilean CC independent members by coalitions (W-Nominate scores)")
#dev.off()

png("figure7.png", units="in", width=7, height=4, res=900)
f7
dev.off()

##### 
# Brazil

#Load the data
library(foreign)
data <- read.dta("anc.dta") # wide form

# Change Small parties
data$party <- ifelse(data$party=="Dir", "SRight",
                     ifelse(data$party=="Esq", "SLeft",data$party))

# Define object rollcall
votes<- (data[,4:length(data[1,])])
my.roll<-rollcall(votes, yea=1, nay=6, missing=9,notInLegis=0, legis.names=data$id, legis.data=data[,1:3])


#wnom
wnom.br <- wnominate(my.roll, dims=1, minvotes=20, lop=0.025, polarity=c(195), verbose=FALSE)

legis <- wnom.br$legislators
legis <- na.omit(legis)
legis$centrao <- ifelse(legis$centrao=="S","Centrão",
                        "Others")

basic <- legis %>% 
  group_by(centrao) %>%
  summarize(
    mean=mean(coord1D, na.rm=T),
    min=min(coord1D, na.rm=T), 
    max=max(coord1D, na.rm=T), 
    SD=sd(coord1D, na.rm=T))

# Figure  3 - Ideal points of Brazilian ANC members by parties (W-Nominate scores) 
legis$party <- with(legis, reorder(party, coord1D, mean))

f3 <- ggplot(legis, aes(coord1D, party)) + geom_jitter() +
  xlab("") + ylab("") +
  #ggtitle("Ideal Points - ANC Brazil") +
  geom_vline(xintercept = 0.07790644, linetype=2) +
  geom_vline(xintercept = 0, linetype=1) +
  theme_bw()

#tiff("figure3.tiff", units="in", width=6.1, height=4, res=900)
#f3 <- gridExtra::grid.arrange(f3, bottom="Figure  3 - Ideal points of Brazilian ANC members by parties (W-Nominate scores)")
#dev.off()

png("figure3.png", units="in", width=6.1, height=4, res=900)
f3
dev.off()

# Figure 5 - Ideal points of Brazilian ANC members by coalitions (W-Nominate scores)
legis$centrao <- with(legis, reorder(centrao, coord1D, mean))

f5 <- ggplot(legis, aes(coord1D, centrao)) + geom_jitter() +
  xlab("") + ylab("") +
  #ggtitle("Ideal Points - ANC Brazil") +
  geom_vline(xintercept = 0.07790644, linetype=2) +
  geom_vline(xintercept = 0, linetype=1) +
  theme_bw()

#tiff("figure5.tiff", units="in", width=6.5, height=4, res=900)
#f5 <- gridExtra::grid.arrange(f5, bottom="Figure 5 - Ideal points of Brazilian ANC members by coalitions (W-Nominate scores)")
#dev.off()

png("figure5.png", units="in", width=6.5, height=4, res=900)
f5
dev.off()

####
# Committees ANC Brazil
votcom <- read_excel("votcom.xlsx")
votcom$Voto <- ifelse(votcom$Voto=="S",1,
                      ifelse(votcom$Voto=="N",6,
                             ifelse(votcom$Voto=="A",0,NA)))

#
vc1 <- votcom[startsWith(votcom$Id_Comissao, '1'),]
vc1$Id_Comissao <- NULL
vc1$Comissao <- NULL

vc1 <- vc1 %>%
  pivot_wider(names_from = Id_votacao_comissao,
              values_from = Voto)

r1 <-rollcall(vc1[,4:51], yea=1, nay=6, missing=0, notInLegis=NA, legis.names=vc1$ID, legis.data=vc1[,1:3])

w1 <- wnominate(r1, dims=1, minvotes=5, lop=0.025, polarity=33, verbose=FALSE)

w1 <- w1$legislators
w1 <- na.omit(w1)

w1 <- w1 %>% 
  group_by(Partido) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Direitos e Garantias",
         english="Rights")


#
vc2 <- votcom[startsWith(votcom$Id_Comissao, '2'),]
vc2$Id_Comissao <- NULL
vc2$Comissao <- NULL

vc2 <- vc2 %>%
  pivot_wider(names_from = Id_votacao_comissao,
              values_from = Voto)

r2 <-rollcall(vc2[,4:length(vc2)], yea=1, nay=6, missing=0, notInLegis=NA, legis.names=vc2$ID, legis.data=vc2[,1:3])

w2 <- wnominate(r2, dims=1, minvotes=5, lop=0.025, polarity=35, verbose=FALSE)

w2 <- w2$legislators
w2 <- na.omit(w2)

w2 <- w2 %>% 
  group_by(Partido) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Organização do Estado",
         english="State Organization")

basic <- bind_rows(w1,w2)

#
vc3 <- votcom[startsWith(votcom$Id_Comissao, '3'),]
vc3$Id_Comissao <- NULL
vc3$Comissao <- NULL

vc3 <- vc3 %>%
  pivot_wider(names_from = Id_votacao_comissao,
              values_from = Voto)

r3 <-rollcall(vc3[,4:length(vc3)], yea=1, nay=6, missing=0, notInLegis=NA, legis.names=vc3$ID, legis.data=vc3[,1:3])

w3 <- wnominate(r3, dims=1, minvotes=5, lop=0.025, polarity=77, verbose=FALSE)

w3 <- w3$legislators
w3 <- na.omit(w3)

w3 <- w3 %>% 
  group_by(Partido) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Poderes e Sistema de Governo",
         english="Powers and System of Government")

basic <- bind_rows(basic,w3)

#
vc4 <- votcom[startsWith(votcom$Id_Comissao, '4'),]
vc4$Id_Comissao <- NULL
vc4$Comissao <- NULL

vc4 <- vc4 %>%
  pivot_wider(names_from = Id_votacao_comissao,
              values_from = Voto)

r4 <-rollcall(vc4[,4:length(vc4)], yea=1, nay=6, missing=0, notInLegis=NA, legis.names=vc4$ID, legis.data=vc4[,1:3])

w4 <- wnominate(r4, dims=1, minvotes=5, lop=0.025, polarity=33, verbose=FALSE)

w4 <- w4$legislators
w4 <- na.omit(w4)

w4 <- w4 %>% 
  group_by(Partido) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Organização Eleitoral e Partidária",
         english="Elections and Parties")

basic <- bind_rows(basic,w4)

#
vc5 <- votcom[startsWith(votcom$Id_Comissao, '5'),]
vc5$Id_Comissao <- NULL
vc5$Comissao <- NULL

vc5 <- vc5 %>%
  pivot_wider(names_from = Id_votacao_comissao,
              values_from = Voto)

r5 <-rollcall(vc5[,4:length(vc5)], yea=1, nay=6, missing=0, notInLegis=NA, legis.names=vc5$ID, legis.data=vc5[,1:3])

w5 <- wnominate(r5, dims=1, minvotes=5, lop=0.025, polarity=5, verbose=FALSE)

w5 <- w5$legislators
w5 <- na.omit(w5)

w5 <- w5 %>% 
  group_by(Partido) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Sistema Tributário",
         english="Taxation")

basic <- bind_rows(basic,w5)

#
vc6 <- votcom[startsWith(votcom$Id_Comissao, '6'),]
vc6$Id_Comissao <- NULL
vc6$Comissao <- NULL

vc6 <- vc6 %>%
  pivot_wider(names_from = Id_votacao_comissao,
              values_from = Voto)

r6 <-rollcall(vc6[,4:length(vc6)], yea=1, nay=6, missing=0, notInLegis=NA, legis.names=vc6$ID, legis.data=vc6[,1:3])

w6 <- wnominate(r6, dims=1, minvotes=5, lop=0.025, polarity=15, verbose=FALSE)

w6 <- w6$legislators
w6 <- na.omit(w6)

w6 <- w6 %>% 
  group_by(Partido) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Ordem Econômica",
         english="Economy")

basic <- bind_rows(basic,w6)

#
vc7 <- votcom[startsWith(votcom$Id_Comissao, '7'),]
vc7$Id_Comissao <- NULL
vc7$Comissao <- NULL

vc7 <- vc7 %>%
  pivot_wider(names_from = Id_votacao_comissao,
              values_from = Voto)

r7 <-rollcall(vc7[,4:length(vc7)], yea=1, nay=6, missing=0, notInLegis=NA, legis.names=vc7$ID, legis.data=vc7[,1:3])

w7 <- wnominate(r7, dims=1, minvotes=5, lop=0.025, polarity=78, verbose=FALSE)

w7 <- w7$legislators
w7 <- na.omit(w7)

w7 <- w7 %>% 
  group_by(Partido) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Ordem Social",
         english="Social Order")

basic <- bind_rows(basic,w7)

#
vc8 <- votcom[startsWith(votcom$Id_Comissao, '8'),]
vc8$Id_Comissao <- NULL
vc8$Comissao <- NULL

vc8 <- vc8 %>%
  pivot_wider(names_from = Id_votacao_comissao,
              values_from = Voto)

r8 <-rollcall(vc8[,4:length(vc8)], yea=1, nay=6, missing=0, notInLegis=NA, legis.names=vc8$ID, legis.data=vc8[,1:3])

w8 <- wnominate(r8, dims=1, minvotes=5, lop=0.025, polarity=1, verbose=FALSE)

w8 <- w8$legislators
w8 <- na.omit(w8)

w8 <- w8 %>% 
  group_by(Partido) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Família, Educação, Cultura e Esportes",
         english="Family, Education, Culture, and Sports")

basic <- bind_rows(basic,w8)

# C. Sistematização
votcs <- read_excel("votcsist.xlsx")
votcs$Voto <- ifelse(votcs$Voto %in% c("S","s"),1,
                      ifelse(votcs$Voto=="N",6,
                             ifelse(votcs$Voto=="A",0,NA)))
votcs[,1]<-NULL
votcs <- na.omit(votcs)

votcs <- votcs %>%
  pivot_wider(names_from = votacao,
              values_from = Voto)

rcs <-rollcall(votcs[,3:length(votcs)], yea=1, nay=6, missing=0, notInLegis=NA, legis.names=votcs$ID, legis.data=votcs[,1:2])

wcs <- wnominate(rcs, dims=1, minvotes=5, lop=0.025, polarity=15, verbose=FALSE)

wcs <- wcs$legislators
wcs <- na.omit(wcs)

wcs <- rename(wcs, "Partido" = "Sigla_Partido")

wcs <- wcs %>% 
  group_by(Partido) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Sistematização",
         english="Systematization")

basic <- bind_rows(basic,wcs)

# Figure 1 - Parties' ideal points in the Brazilian ANC's Committees (mean W-Nominate scores)

basic <- rename(basic, Mean = mean)
basic$Partido <- with(basic, reorder(Partido, Mean, mean))

basic$english_f = factor(basic$english, levels=c("Rights", "State Organization", "Powers and System of Government",
                                             "Elections and Parties", "Taxation", "Economy",
                                             "Social Order", "Family, Education, Culture, and Sports", "Systematization"))

f1 <- ggplot(basic, aes(y=Partido, x=Mean, xmin = Mean-sd, xmax = Mean+sd)) + geom_pointrange() +
  geom_vline(xintercept = 0, linetype=1) +
  xlab("") + ylab("") + facet_wrap( ~ english_f, ncol=3) +
  theme_bw()

png("figure1.png", units="in", width=7.2, height=7.2, res=900)
f1
dev.off()

##############
### Committees CC Chile
# SP
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Sistema Político")

vot <- vot %>%
  pivot_longer(cols = 7:length(vot),
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)


vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), -1, 
                          ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto)

nominais <- left_join(vot,perfil)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

r <- rollcall(nominais[1:(length(nominais)-14)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[(length(nominais)-13):(length(nominais))])

c.sp <- wnominate(r, dims=1, minvotes=20, lop=0.025, polarity=c(12), verbose=FALSE)

legis <- c.sp$legislators
legis <- na.omit(legis)

legis$party <- with(legis, reorder(party, coord1D, mean))

c.sp <- legis %>% 
  group_by(party) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Sistema Político",
         english="Political System")

# PC
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Principios Constitucionales")

vot <- vot %>%
  pivot_longer(cols = 7: length(vot),
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)


vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), -1, 
                          ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto)

nominais <- left_join(vot,perfil)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

r <- rollcall(nominais[1:(length(nominais)-14)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[(length(nominais)-13):(length(nominais))])

c.pc <- wnominate(r, dims=1, minvotes=20, lop=0.025, polarity=c(12), verbose=FALSE)

legis <- c.pc$legislators
legis <- na.omit(legis)

legis$party <- with(legis, reorder(party, coord1D, mean))

c.pc <- legis %>% 
  group_by(party) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Principios Constitucionales",
         english="Constitutional Principles")

basic <- bind_rows(c.sp,c.pc)

# FE
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Forma de Estado")

vot <- vot %>%
  pivot_longer(cols = 7: length(vot),
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)


vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), -1, 
                          ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto)

nominais <- left_join(vot,perfil)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

r <- rollcall(nominais[1:(length(nominais)-14)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[(length(nominais)-14):(length(nominais))])

c.fe <- wnominate(r, dims=1, minvotes=20, lop=0.025, polarity=c(10), verbose=FALSE)

legis <- c.fe$legislators
legis <- na.omit(legis)

legis$party <- with(legis, reorder(party, coord1D, mean))

c.fe <- legis %>% 
  group_by(party) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Forma de Estado",
         english="State Organization")

basic <- bind_rows(basic,c.fe)

# DF
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Derechos Fundamentales")

vot <- vot %>%
  pivot_longer(cols = 7: length(vot),
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)


vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), -1, 
                          ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto)

nominais <- left_join(vot,perfil)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

r <- rollcall(nominais[1:(length(nominais)-14)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[(length(nominais)-13):(length(nominais))])

c.df <- wnominate(r, dims=1, minvotes=20, lop=0.025, polarity=c(2), verbose=FALSE)

legis <- c.df$legislators
legis <- na.omit(legis)

legis$party <- with(legis, reorder(party, coord1D, mean))

c.df <- legis %>% 
  group_by(party) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Derechos Fundamentales",
         english="Rights")

basic <- bind_rows(basic,c.df)

# MA
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Medio Ambiente")

vot <- vot %>%
  pivot_longer(cols = 7: length(vot),
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)


vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), -1, 
                          ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto)

nominais <- left_join(vot,perfil)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

r <- rollcall(nominais[1:(length(nominais)-14)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[(length(nominais)-14):(length(nominais))])

c.ma <- wnominate(r, dims=1, minvotes=20, lop=0.025, polarity=c(10), verbose=FALSE)

legis <- c.ma$legislators
legis <- na.omit(legis)

legis$party <- with(legis, reorder(party, coord1D, mean))

c.ma <- legis %>% 
  group_by(party) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Medio Ambiente",
         english="Environment")

basic <- bind_rows(basic,c.ma)

# SJ
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Sistema de Justicia")

vot <- vot %>%
  pivot_longer(cols = 7: length(vot),
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)


vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), -1, 
                          ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto)

nominais <- left_join(vot,perfil)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

r <- rollcall(nominais[1:(length(nominais)-14)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[(length(nominais)-13):(length(nominais))])

c.sj <- wnominate(r, dims=1, minvotes=20, lop=0.025, polarity=c(12), verbose=FALSE)

legis <- c.sj$legislators
legis <- na.omit(legis)

legis$party <- with(legis, reorder(party, coord1D, mean))

c.sj <- legis %>% 
  group_by(party) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Sistema de Justicia",
         english="Justice System")

basic <- bind_rows(basic,c.sj)

# SC
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Sistemas de Conocimientos")

vot <- vot %>%
  pivot_longer(cols = 7: length(vot),
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)


vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), -1, 
                          ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto)

nominais <- left_join(vot,perfil)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

r <- rollcall(nominais[1:(length(nominais)-14)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[(length(nominais)-14):(length(nominais))])

c.sc <- wnominate(r, dims=1, minvotes=20, lop=0.025, polarity=c(17), verbose=FALSE)

legis <- c.sc$legislators
legis <- na.omit(legis)

legis$party <- with(legis, reorder(party, coord1D, mean))

c.sc <- legis %>% 
  group_by(party) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Sistemas de Conocimientos",
         english="Education, Culture, and Science")

basic <- bind_rows(basic,c.sc)

# PI
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Pueblos Indígenas")

vot <- vot %>%
  pivot_longer(cols = 7: length(vot),
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)


vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), -1, 
                          ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto)

nominais <- left_join(vot,perfil)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

r <- rollcall(nominais[1:(length(nominais)-14)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[(length(nominais)-14):(length(nominais))])

c.pi <- wnominate(r, dims=1, minvotes=8, lop=0.025, polarity=c(26), verbose=FALSE)

legis <- c.pi$legislators
legis <- na.omit(legis)

legis$party <- with(legis, reorder(party, coord1D, mean))

c.pi <- legis %>% 
  group_by(party) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Pueblos Indígenas",
         english="Indigenous Peoples")

basic <- bind_rows(basic,c.pi)

# Armonización
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Armonización")

vot <- vot %>%
  pivot_longer(cols = 7: length(vot),
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)


vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), -1, 
                          ifelse(vot$voto %in% c("Abstencion"), 0, NA)))

vot <- vot[!duplicated(vot), ]

vot <- vot %>%
  pivot_wider(
    names_from = votacion_id,
    values_from = voto)

nominais <- left_join(vot,perfil)

nominais <- nominais %>%
  column_to_rownames(var = "apellido_nombre")

nominais <- rename(nominais, party = tag_partido)

r <- rollcall(nominais[1:(length(nominais)-14)], yea = 1, nay = -1, missing = 0, notInLegis = NA,
              legis.names=nominais$id, legis.data=nominais[(length(nominais)-13):(length(nominais))])

c.ar <- wnominate(r, dims=1, minvotes=20, lop=0.025, polarity=c(7), verbose=FALSE)

legis <- c.ar$legislators
legis <- na.omit(legis)

legis$party <- with(legis, reorder(party, coord1D, mean))

c.ar <- legis %>% 
  group_by(party) %>% 
  summarize(
    mean=mean(coord1D),
    min=min(coord1D), 
    max=max(coord1D), 
    sd=sd(coord1D)) %>%
  mutate(comis="Armonización",
         english="Systematization")

basic <- bind_rows(basic,c.ar)

# Figure 2 - Parties' ideal points in the Chilean CC's Committees (mean W-Nominate scores)

basic <- rename(basic, Mean = mean)
basic$party <- with(basic, reorder(party, Mean, mean))


basic$english_f = factor(basic$english, levels=c("Rights", "State Organization", "Constitutional Principles",
                                                 "Political System", "Justice System", "Environment",
                                                 "Indigenous Peoples", "Education, Culture, and Science", "Systematization"))


f2 <- ggplot(basic, aes(y=party, x=Mean, xmin = Mean-sd, xmax = Mean+sd)) + geom_pointrange() +
  geom_vline(xintercept = 0, linetype=1) +
  xlab("") + ylab("") + facet_wrap( ~ english_f, ncol=3) +
  theme_bw()

png("figure2.png", units="in", width=7.2, height=7.2, res=900)
f2
dev.off()

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

##read data
OECD_health <- read.csv(".\\data\\health_OECD.csv") # health spend data
OECD_education <- read.csv(".\\data\\Education_OECD.csv") # education spend data
OECD_mortality <- read.csv(".\\data\\OECD_mortality.csv") ## treatable and preventable mortality
PIRLS <- read.csv(".\\data\\PIRLS_2021.csv")
TIMSS_S8 <- read.csv(".\\data\\TIMSS_S_8_2019.csv")
TIMSS_M8 <- read.csv(".\\data\\TIMSS_M_8_2019.csv")
TIMSS_S4 <- read.csv(".\\data\\TIMSS_S_4_2019.csv")
TIMSS_M4 <- read.csv(".\\data\\TIMSS_M_4_2019.csv")
PISA_R <- read.csv(".\\data\\PISA_reading.csv")
PISA_M <- read.csv(".\\data\\PISA_maths.csv")
PISA_S <- read.csv(".\\data\\PISA_reading.csv")

##data frames

# Define JBM peers
JBM_peers <- c("GBR", "AUT", "CAN", "DNK", "DEU", "FIN", "FRA", "NLD", "NOR", "SWE", "CHE", "USA")  # I added AUS

# JBM compulsory health spending (%GDP)

OECD_health_comp <- OECD_health[OECD_health$SUBJECT == "COMPULSORY" & OECD_health$MEASURE == "PC_GDP" & OECD_health$TIME %in% 2000:2019,]
OECD_health_comp1 <- OECD_health_comp[,c(1,6:7)]
OECD_health_JBM <- OECD_health_comp1[OECD_health_comp1$LOCATION %in% JBM_peers,]
OECD_health_TAB <- OECD_health_comp1[OECD_health_comp1$LOCATION %in% c("GBR", "POL", "SVN"),]
OECD_health_JBM
OECD_health_TAB

# Total health (%GDP)
OECD_health_total <- OECD_health[OECD_health$SUBJECT == "TOT" & OECD_health$MEASURE == "PC_GDP"  & OECD_health$LOCATION  %in% JBM_peers & OECD_health$TIME %in% 2000:2019,]
OECD_health_total <- OECD_health_total[,c(1,6:7)]



# Total primary to non-non-tertiary education spending (%GDP)
primary_ed <- OECD_education[OECD_education$LOCATION %in% JBM_peers & OECD_education$SUBJECT == "PRY_NTRY" & OECD_education$MEASURE == "PC_GDP",]
primary_ed <- primary_ed[,c(1, 6, 7)]

# Preventable mortality

preventable <- OECD_mortality[OECD_mortality$Variable == "Preventable mortality" & OECD_mortality$Measure == "Deaths per 100 000 population (standardised rates)",]
preventable <- preventable[,c(5,7,9)]
preventable_AT <- preventable[preventable$COU %in% c("GBR", "POL", "SVN"),]
preventable_JBM <- preventable[preventable$COU %in% JBM_peers, ]

# Treatable mortality

treatable <- OECD_mortality[OECD_mortality$Variable == "Treatable mortality" & OECD_mortality$Measure == "Deaths per 100 000 population (standardised rates)",]
treatable <- treatable[,c(5,7,9)]
treatable_TabPeers <- treatable[treatable$COU %in% c("GBR", "POL", "SVN"),]
treatable_JBM <- treatable[treatable$COU %in% JBM_peers, ]

# International Education Rankings (PIRLS, TIMMS, PISA)

PIRLS1 <- PIRLS[-c(1:7, 44, 48:61),-c(1,2,4,6,8,10,12:29)]
PIRLS1[1,1] <- "Country"
names <- PIRLS1[1,]
PIRLS2 <- PIRLS1[-1,]
colnames(PIRLS2) <- names
rownames(PIRLS2) <- NULL

JBM_peers_proxy <- c("England ⋈", "Austria", "Denmark", "Germany", "Finland", "France", "Netherlands", "Norway (5)", "Sweden")
PIRLS_JBM <- PIRLS2[PIRLS2$Country %in% JBM_peers_proxy, ]

TIMSS_S8_1 <- TIMSS_S8[-c(1:4,6,51:111),-c(1,4,6,8,10,12,14,16, 18:30),]
names <- TIMSS_S8_1[1,]
TIMSS_S8_2 <- TIMSS_S8_1[-1,]
colnames(TIMSS_S8_2) <- names
rownames(TIMSS_S8_2) <- NULL

TIMSS_M8_1 <- TIMSS_M8[-c(1:4,6,51:111),-c(1,4,6,8,10,12,14,16, 18:30),]
names <- TIMSS_M8_1[1,]
TIMSS_M8_2 <- TIMSS_M8_1[-1,]
colnames(TIMSS_M8_2) <- names
rownames(TIMSS_M8_2) <- NULL

TIMSS_S4_1 <- TIMSS_S4[-c(1:4,6,51:111),-c(1,2,5,7,9,11,13,15, 16:28),]
names <- TIMSS_S4_1[1,]
TIMSS_S4_2 <- TIMSS_S4_1[-1,]
colnames(TIMSS_S4_2) <- names
rownames(TIMSS_S4_2) <- NULL

TIMSS_M4_1 <- TIMSS_M4[-c(1:4,6,51:111),-c(1,4,6,8,10,12,14,16, 18:30),]
names <- TIMSS_M4_1[1,]
TIMSS_M4_2 <- TIMSS_M4_1[-1,]
colnames(TIMSS_M4_2) <- names
rownames(TIMSS_M4_2) <- NULL

PISA_R1 <- PISA_R[-c(1:8),1:3]
colnames(PISA_R1) <- PISA_R1[1,]
PISA_R2 <- PISA_R1[-1,]
PISA_R3 <- tidyr::fill(PISA_R2, "Year/Study")


PISA_R1 <- PISA_R[-c(1:8), 1:3]
colnames(PISA_R1) <- c("Year/Study", "Country", "Value")
PISA_R2 <- PISA_R1[-1,]
PISA_R2$`Year/Study`[PISA_R2$`Year/Study` == ""] <- NA
PISA_R3 <- PISA_R2 %>%
  fill("Year/Study", .direction = "down")

PISA_S1 <- PISA_S[-c(1:8), 1:3]
colnames(PISA_S1) <- c("Year/Study", "Country", "Value")
PISA_S2 <- PISA_S1[-1,]
PISA_S2$`Year/Study`[PISA_S2$`Year/Study` == ""] <- NA
PISA_S3 <- PISA_S2 %>%
  fill("Year/Study", .direction = "down")
PISA_S3

PISA_M1 <- PISA_M[-c(1:8), 1:3]
colnames(PISA_M1) <- c("Year/Study", "Country", "Value")
PISA_M2 <- PISA_M1[-1,]
PISA_M2$`Year/Study`[PISA_M2$`Year/Study` == ""] <- NA
PISA_M3 <- PISA_M2 %>%
  fill("Year/Study", .direction = "down")
PISA_M3

## graphs

#JBM

library(ggplot2)
library(dplyr)

# Read data
OECD_health <- read.csv(".\\data\\health_OECD.csv") # health spend data
OECD_education <- read.csv(".\\data\\Education_OECD.csv") # education spend data
OECD_mortality <- read.csv(".\\data\\OECD_mortality.csv")

# Define JBM peers
JBM_peers <- c("GBR", "AUT", "CAN", "DNK", "DEU", "FIN", "FRA", "NLD", "NOR", "SWE", "CHE", "USA")  # I added AUS

# JBM compulsory health spending (%GDP)

OECD_health_JBM <- OECD_health[OECD_health$SUBJECT == "COMPULSORY" & OECD_health$MEASURE == "PC_GDP"  & OECD_health$LOCATION  %in% JBM_peers & OECD_health$TIME %in% 2000:2019,]
OECD_health_JBM <- OECD_health_JBM[,c(1,6:7)]

# Total health (%GDP)
OECD_health_total <- OECD_health[OECD_health$SUBJECT == "TOT" & OECD_health$MEASURE == "PC_GDP"  & OECD_health$LOCATION  %in% JBM_peers & OECD_health$TIME %in% 2000:2019,]
OECD_health_total <- OECD_health_total[,c(1,6:7)]


# Total primary education spending (%GDP)
primary_ed <- OECD_education[OECD_education$LOCATION %in% JBM_peers & OECD_education$SUBJECT == "PRY" & OECD_education$MEASURE == "PC_GDP",]
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


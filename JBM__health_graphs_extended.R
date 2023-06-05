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
PISA_S <- read.csv(".\\data\\PISA_science.csv")

##data frames

# Define JBM peers
JBM_peers <- c("GBR", "AUT", "CAN", "DNK", "DEU", "FIN", "FRA", "NLD", "NOR", "SWE", "CHE")

# JBM compulsory health spending (%GDP)
OECD_health_comp <- OECD_health[OECD_health$SUBJECT == "COMPULSORY" & OECD_health$MEASURE == "PC_GDP" & OECD_health$TIME %in% 2000:2019,]
OECD_health_comp1 <- OECD_health_comp[,c(1,6:7)]
OECD_health_JBM <- OECD_health_comp1[OECD_health_comp1$LOCATION %in% JBM_peers,]
OECD_health_TAB <- OECD_health_comp1[OECD_health_comp1$LOCATION %in% c("GBR", "POL", "SVN"),]

agg_data <- OECD_health_JBM %>%
  group_by(TIME) %>%
  summarise(mean_value = mean(Value),
            min_value = min(Value),
            max_value = max(Value))

ggplot() +
  geom_ribbon(data = agg_data, aes(x = TIME, ymin = min_value, ymax = max_value),
              fill = "darkgrey", alpha = 0.2) +
  geom_line(data = OECD_health_JBM, aes(x = TIME, y = Value, color = LOCATION), size = 1.5) +
  geom_line(data = agg_data, aes(x = TIME, y = mean_value), linetype = "dashed", color = "black", size = 1.5) +
  geom_point(data = OECD_health_JBM, aes(x = TIME, y = Value, color = LOCATION)) +
  scale_color_manual(values = c("GBR" = "red", "grey")) +
  labs(x = "Year", y = "Value") +
  ggtitle("JBM graph -- compulsory spending on health (%GDP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(OECD_health_TAB, aes(x = TIME, y = Value, color = LOCATION, group = LOCATION)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Value") +
  ggtitle("Alex Tabarrok -- compulsory spending on health (%GDP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  

# Total health (%GDP)
OECD_health_total <- OECD_health[OECD_health$SUBJECT == "TOT" & OECD_health$MEASURE == "PC_GDP" & OECD_health$TIME %in% 2000:2019,]
OECD_health_total_JBM <- OECD_health_total[OECD_health_total$LOCATION  %in% JBM_peers, ]
OECD_health_total_JBM <- OECD_health_total_JBM[,c(1,6:7)]
OECD_health_TAB <- OECD_health_total[OECD_health_total$LOCATION %in% c("GBR", "POL", "SVN"),]

agg_data <- OECD_health_total_JBM %>%
  group_by(TIME) %>%
  summarise(mean_value = mean(Value),
            min_value = min(Value),
            max_value = max(Value))

ggplot() +
  geom_ribbon(data = agg_data, aes(x = TIME, ymin = min_value, ymax = max_value),
              fill = "darkgrey", alpha = 0.2) +
  geom_line(data = OECD_health_total_JBM, aes(x = TIME, y = Value, color = LOCATION), size = 1.5) +
  geom_line(data = agg_data, aes(x = TIME, y = mean_value), linetype = "dashed", color = "black", size = 1.5) +
  geom_point(data = OECD_health_total_JBM, aes(x = TIME, y = Value, color = LOCATION)) +
  scale_color_manual(values = c("GBR" = "red", "grey")) +
  labs(x = "Year", y = "Value") +
  ggtitle("JBM peers -- total spending on health (%GDP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggplot(OECD_health_TAB, aes(x = TIME, y = Value, color = LOCATION, group = LOCATION)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Value") +
  ggtitle("Alex Tabarrok peers -- total spending on health (%GDP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Total primary to non-non-tertiary education spending (%GDP)
JBM_peers_proxy <- JBM_peers_proxy <- c("GBR", "CAN", "DNK", "FIN", "FRA", "NLD", "NOR", "SWE", "USA")
ed <- OECD_education[OECD_education$LOCATION %in% JBM_peers_proxy & OECD_education$SUBJECT == "PRY_NTRY" & OECD_education$MEASURE == "PC_GDP" & OECD_education$TIME %in% 2005:2019,]
ed <- ed[,c(1, 6, 7)]

agg_data <- ed %>%
  group_by(TIME) %>%
  summarise(mean_value = mean(Value),
            min_value = min(Value),
            max_value = max(Value))

ggplot() +
  geom_ribbon(data = agg_data, aes(x = TIME, ymin = min_value, ymax = max_value),
              fill = "darkgrey", alpha = 0.2) +
  geom_line(data = ed, aes(x = TIME, y = Value, color = LOCATION), size = 1.5) +
  geom_line(data = agg_data, aes(x = TIME, y = mean_value), linetype = "dashed", color = "black", size = 1.5) +
  geom_point(data = ed, aes(x = TIME, y = Value, color = LOCATION)) +
  scale_color_manual(values = c("GBR" = "red", "grey")) +
  labs(x = "Year", y = "Value") +
  ggtitle("JBM peers -- Total primary to non-non-tertiary education spending (%GDP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



#Total primary education spending 

JBM_peers_proxy <- JBM_peers <- c("GBR", "CAN", "DNK", "FIN", "FRA", "NLD", "NOR", "SWE", "USA")
primary_ed <- OECD_education[OECD_education$LOCATION %in% JBM_peers_proxy & OECD_education$SUBJECT == "PRY" & OECD_education$MEASURE == "PC_GDP" & OECD_education$TIME %in% 2005:2019,]
primary_ed <- primary_ed[,c(1,6,7)]
primary_ed

agg_data <- primary_ed %>%
  group_by(TIME) %>%
  summarise(mean_value = mean(Value),
            min_value = min(Value),
            max_value = max(Value))

ggplot() +
  geom_ribbon(data = agg_data, aes(x = TIME, ymin = min_value, ymax = max_value),
              fill = "darkgrey", alpha = 0.2) +
  geom_line(data = primary_ed, aes(x = TIME, y = Value, color = LOCATION), size = 1.5) +
  geom_line(data = agg_data, aes(x = TIME, y = mean_value), linetype = "dashed", color = "black", size = 1.5) +
  geom_point(data = primary_ed, aes(x = TIME, y = Value, color = LOCATION)) +
  scale_color_manual(values = c("GBR" = "red", "grey")) +
  labs(x = "Year", y = "Value") +
  ggtitle("JBM peers -- primary education (%GDP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

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
PIRLS2 <- PIRLS2 %>%
  mutate(across(starts_with("20"), as.character))
PIRLS3 <- PIRLS2 %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Value")
JBM_peers_proxy <- c("England ⋈", "Austria", "Denmark", "Germany", "Finland", "France", "Netherlands", "Norway (5)", "Sweden")


TIMSS_S8_1 <- TIMSS_S8[-c(1:4,6,46,51:111),-c(1,2,4,6,8,10,12,14,16, 18:30),]
names <- TIMSS_S8_1[1,]
TIMSS_S8_2 <- TIMSS_S8_1[-1,]
colnames(TIMSS_S8_2) <- names
rownames(TIMSS_S8_2) <- NULL
TIMSS_S8_3 <- TIMSS_S8_2 %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Value")
JBM_peers_proxy <- c("England", "Finland", "France", "Norway (9)", "Sweden", "United States")
TIMSS_S8_JBM <- TIMSS_S8_3[TIMSS_S8_3$Country %in% JBM_peers_proxy, ]

TIMSS_M8_1 <- TIMSS_M8[-c(1:4,6,46,51:111),-c(1,2,4,6,8,10,12,14,16, 18:30),]
names <- TIMSS_M8_1[1,]
TIMSS_M8_2 <- TIMSS_M8_1[-1,]
colnames(TIMSS_M8_2) <- names
rownames(TIMSS_M8_2) <- NULL
JBM_peers_proxy <- c("England", "Finland", "France", "Norway (9)", "Sweden", "United States")
TIMSS_M8_3 <- TIMSS_M8_2 %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Value")
JBM_peers_proxy <- c("England", "Finland", "France", "Norway (9)", "Sweden", "United States")
TIMSS_M8_JBM <- TIMSS_M8_3[TIMSS_M8_3$Country %in% JBM_peers_proxy, ]

TIMSS_S4_1 <- TIMSS_S4[-c(1:4,6,51:111),-c(1,2,5,7,9,11,13,15, 16:28),]
names <- TIMSS_S4_1[1,]
TIMSS_S4_2 <- TIMSS_S4_1[-1,]
colnames(TIMSS_S4_2) <- names
rownames(TIMSS_S4_2) <- NULL
TIMSS_S4_3 <- TIMSS_S4_2 %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Value")
JBM_peers_proxy <- c("England", "Finland", "France", "Norway (9)", "Sweden", "United States")
TIMSS_S4_JBM <- TIMSS_S4_3[TIMSS_S4_3$Country %in% JBM_peers_proxy, ]

TIMSS_M4_1 <- TIMSS_M4[-c(1:4,6,51:111),-c(1,2,4,6,8,10,12,14,16, 18:30),]
names <- TIMSS_M4_1[1,]
TIMSS_M4_2 <- TIMSS_M4_1[-1,]
colnames(TIMSS_M4_2) <- names
rownames(TIMSS_M4_2) <- NULL
TIMSS_M4_3 <- TIMSS_M4_2 %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Value")
JBM_peers_proxy <- c("England", "Finland", "France", "Norway (9)", "Sweden", "United States")
TIMSS_M4_JBM <- TIMSS_M4_3[TIMSS_M4_3$Country %in% JBM_peers_proxy, ]

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
JBM_peers <- c("United Kingdom", "Austria", "Canada", "Denmark", "Germany", "Finland", "France", "Netherlands", "Norway", "Sweden", "Switzerland", "United States")
PISA_R_JBM <- PISA_R3[PISA_R3$Country %in% JBM_peers,]

PISA_S1 <- PISA_S[-c(1:8), 1:3]
colnames(PISA_S1) <- c("Year/Study", "Country", "Value")
PISA_S2 <- PISA_S1[-1,]
PISA_S2$`Year/Study`[PISA_S2$`Year/Study` == ""] <- NA
PISA_S3 <- PISA_S2 %>%
  fill("Year/Study", .direction = "down")
JBM_peers <- c("United Kingdom", "Austria", "Canada", "Denmark", "Germany", "Finland", "France", "Netherlands", "Norway", "Sweden", "Switzerland", "United States")
PISA_S_JBM <- PISA_S3[PISA_S3$Country %in% JBM_peers,]

PISA_M1 <- PISA_M[-c(1:8), 1:3]
colnames(PISA_M1) <- c("Year/Study", "Country", "Value")
PISA_M2 <- PISA_M1[-1,]
PISA_M2$`Year/Study`[PISA_M2$`Year/Study` == ""] <- NA
PISA_M3 <- PISA_M2 %>%
  fill("Year/Study", .direction = "down")
JBM_peers <- c("United Kingdom", "Austria", "Canada", "Denmark", "Germany", "Finland", "France", "Netherlands", "Norway", "Sweden", "Switzerland", "United States")
PISA_M_JBM <- PISA_M3[PISA_M3$Country %in% JBM_peers,]

# Coe reproduced in Dominic Cummings blog post on 

UK_PISA_M <- PISA_M3[PISA_M3$Country == "United Kingdom", ]
UK_PISA_M$Study <- "UK PISA Maths"
colnames(UK_PISA_M)[1] <- "Year"
UK_PISA_M <- UK_PISA_M[, c("Country", "Year", "Value", "Study")]

UK_PISA_S <- PISA_S3[PISA_M3$Country == "United Kingdom", ]
UK_PISA_S$Study <- "UK PISA Science"
colnames(UK_PISA_S)[1] <- "Year"
UK_PISA_S <- UK_PISA_S[, c("Country", "Year", "Value", "Study")]

UK_PISA_R <- PISA_R3[PISA_M3$Country == "United Kingdom", ]
UK_PISA_R$Study <- "UK PISA Reading"
colnames(UK_PISA_R)[1] <- "Year"
UK_PISA_R <- UK_PISA_R[, c("Country", "Year", "Value", "Study")]

UK_PISA_R

England_PIRLS <-  PIRLS3[PIRLS3$Country == "England ⋈", ]
England_PIRLS$Study <- "England PIRLS"
England_PIRLS

England_TIMSS_M_4 <- TIMSS_M4_3[TIMSS_M4_3$Country == "England", ] 
England_TIMSS_M_4$Study <- "England TIMSS Maths Age 10"

England_TIMSS_M_8 <- TIMSS_M8_3[TIMSS_M8_3$Country == "England", ] 
England_TIMSS_M_8$Study <- "England TIMSS Maths Age 14"

England_TIMSS_S_4 <- TIMSS_S4_3[TIMSS_S4_3$Country == "England", ]
England_TIMSS_S_4$Study <- "England TIMSS Science Age 10"

England_TIMSS_S_8 <- TIMSS_S8_3[TIMSS_S8_3$Country == "England", ] 
England_TIMSS_S_8$Study <- "England TIMSS Science Age 14"

COE <- rbind(UK_PISA_M, UK_PISA_R, UK_PISA_S, England_PIRLS, England_TIMSS_M_4, England_TIMSS_M_8, England_TIMSS_S_4, England_TIMSS_S_8)
COE <- COE[,-1]
rownames(COE) <- NULL
COE <- COE[-c(7,20,21, 32),]
COE <- COE[order(COE$Year),]

ggplot(COE, aes(x = Year, y = Value, color = Study, group = Study)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Value") +
  ggtitle("Robert Coe graph reproduced in Dominic Cummings blog -- extended for Coalition/Con governments") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
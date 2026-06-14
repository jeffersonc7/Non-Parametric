# Proportion Test 

# Ho) Proportion (people with pension) = 10%
# H1) The opposite 

# Libraries --------------------------------------------------------------------
library(rio)
library(dplyr)

# Data -------------------------------------------------------------------------
data <- import("/home/c4975224/Programming/Non_Parametric/No Parametrica/base/06_ecv6r_otros_ingresos.csv") %>% 
  filter(REGION == 'Sierra')

data$IB0101 <- factor(data$IB0101,
                      levels = c('Si','No'),
                      labels = c('Receives','Does not receive'))
head(data,5)

# Descriptive Analysis ---------------------------------------------------------

library('descr')

# Frequency Table 
fcond <-  freq(data$IB0101,plot= FALSE);fcond
# Cross Table
table <-  crosstab(data$IB0101, data$PROVINCIA,
                   plot = FALSE, 
                   dnn = c('Pension/retirement','Province'));table

# Graphics 
par(mfrow = c(1,2))

plot(fcond,
     main = 'People with pension',
     ylab = 'Frecuency',
     col = rainbow(2))

plot(table,
     main = 'Pension/Retirement for Provincie',
     ylab = 'Pension / Retirement',
     col = rainbow(2))

# Step by Step TEST ------------------------------------------------------------

# Binary Variable

data$b <-  ifelse( is.na(data$IB0101) | data$IB0101 == "", NA,
                   ifelse(data$IB0101 == 'Receives',1,0))

# Estimate Proportion
tot_p <- mean( data$b, na.rm= TRUE); tot_p
tot_q <- 1 - tot_p; tot_q
n <- sum(!is.na(data$b)) ; n 

# Statistic 
t_calculated <- ( tot_p - 0.10 ) /sqrt(tot_p * tot_q /n); t_calculated
t_critical <- qt(0.05/2, n-1) ; t_critical

# P-value 
p_value <-  (1- pt(abs(t_calculated), n - 1)); p_value


# Direct Function --------------------------------------------------------------

test <- t.test(data$b, 
               alternative = 'two.sided',
               mu = 0.10 ,
               conf.level = 0.95)

test
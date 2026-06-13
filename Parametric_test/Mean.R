# Mean Test 

# Ho) mean(Income) = 500
# H1) The opposite

# Libraries --------------------------------------------------------------------
library(rio)
library(dplyr)

# Data -------------------------------------------------------------------------
data <- import("/home/c4975224/Programming/Non_Parametric/No Parametrica/base/06_ecv6r_otros_ingresos.csv") %>% 
  filter(IB0101 == 'Si')

data$IB0102 = as.numeric(data$IB0102)
head(data,5)

# Graphical Analysis ----------------------------------------------------------- 
par(mfrow = c(2,2))

plot(table(data$IB0102),
     main = 'Histogram', 
     ylab = 'Frecuency', 
     xlab = 'Income')

plot(density(data$IB0102, na.rm = TRUE),
     main = 'Density Income')

qqnorm(data$IB0102,
       main = 'QQ Plot')
qqline(data$IB0102,
       col = 'red')
boxplot(data$IB0102,
        main = 'Boxplot - Income',
        ylabel = 'Frecuency ',
        clabel = 'Income')

# Step by Step TEST ------------------------------------------------------------

# Descriptive Statistics

mean = mean(data$IB0102, na.rm = TRUE); mean 
stan_dev = sd(data$IB0102,na.rm = TRUE); stan_dev
n = nrow(data); n

# Statistic (T)
t_calculated = (mean - 500 ) / (stan_dev /sqrt(n)); t_calculated
t_critical = qt(0.05/2, n -1); t_critical

# P-value 
P_value = (1 - pt(abs(t_calculated), n-1)) * 2; P_value


# Direct Function --------------------------------------------------------------
test <-  t.test(data$IB0102,
                alternative = "two.sided",
                mu = 500, 
                conf.level = 0.95)
print(test)
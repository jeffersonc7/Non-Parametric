# Difference in Means Test 

# Ho) mean(child support in Pichincha) = mean(child support in Guayas)
# H1) The opposite

# Libraries --------------------------------------------------------------------
library(rio)
library(dplyr)

# Data -------------------------------------------------------------------------
data <- import("/home/c4975224/Programming/Non_Parametric/No Parametrica/base/06_ecv6r_otros_ingresos.csv") %>% 
  select(PROVINCIA,IB0102) %>%  
  filter(PROVINCIA %in% c('Pichincha','Guayas'), 
         !(IB0102 %in% c("",'00 ""No informa""')))

data$IB0102 <-  as.numeric(data$IB0102)
head(data,5)

# Graphical Analysis ----------------------------------------------------------- 
par(mfrow = c(2,2))

plot(table(data$IB0102),
     main = "Histogram Child support ",
     ylab = "Frequency",
     xlab = "Child Support ($)")

plot(density(data$IB0102),
     main = "Density Child support")

qqnorm(data$IB0102,
       main = "QQplot Child Support")
qqline(data$IB0102,
       col = 'red')

boxplot(IB0102~PROVINCIA, data = data, 
        main = 'Box plot Child Suppport ', 
        xlab = 'Province',
        ylab = 'Child Support')

# Diagnostic Tests --------------------------------------------------------------

# Normality 
# H0) The data follow an approximately normal distribution 
# H1) The opposite 

library(tseries)

jarque.bera.test(data$IB0102)

# Step by Step TEST ------------------------------------------------------------

# mean for groups 
mean_1 = mean(data$IB0102[data$PROVINCIA == 'Pichincha'] ); mean_1 
mean_2 = mean(data$IB0102[data$PROVINCIA == 'Guayas'] ); mean_2

# variance
s_1 = sd(data$IB0102[data$PROVINCIA == 'Pichincha'] )^ 2 
s_2 = sd(data$IB0102[data$PROVINCIA == 'Guayas']) ^ 2

# Sample Size 
n_1 = sum(data$PROVINCIA == 'Pichincha')
n_2 = sum(data$PROVINCIA == 'Guayas')

# statistic (T)
t_calculated = (mean_1 - mean_2) / sqrt((s_1/n_1) + (s_2/n_2)) ;t_calculated

# P-value
pvalue = (1 - pt(abs(t_calculated), n_1 + n_2 -2 )) * 2 ; pvalue


# Direct Function --------------------------------------------------------------
test <-  t.test(IB0102 ~ PROVINCIA,
                data = data,
                alternative = "two.sided",
                mu = 0, 
                conf.level = 0.95)

print(test)

p_needed <-
  c("knitr",
    "MASS",
    "foreign"
  )
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)
library(stargazer)

## Data preparation

dta <- read.csv("~/Downloads/data_essay19.csv")

data1 <- na.omit(dta)
names(data1)

## Model Fit testing 
par(mfrow=c(1, 1))
plot(data1$year,
     data1$committee_staff_size,
     main = "Changes of committee staff size from 1905- 2009",
     xlab = "Year",
     ylab =  "Committee staff size",
     bty = "n",
     las = 1,
     pch = 1,
     col= "green"
)

m1 <- glm(executive_orders ~ divided + log(data1$committee_staff_size), data= data1, family= "poisson")
m2 <- glm.nb(executive_orders ~ divided + log(data1$committee_staff_size), data= data1, control= glm.control(maxit=100))
L1<- logLik(m1)
L2<- logLik(m2) 
LRT <--2 * L1 + 2 * L2
LRT > qchisq(0.95, df = 1)
## We know that that negative binomial model is statistically significanly a better fit model than the Poisson model. 
## Hence, we would be using the negative binomial models in the following section. 

m3<- glm.nb(executive_orders~divided+ log(data1$legislative_expenditures), data=data1, control=glm.control((maxit=100)))
m4<- glm.nb(executive_orders~divided+ log(data1$committee_staff_size) ,data=data1,control=glm.control((maxit=100)))
L3<- logLik(m3)
L4<- logLik(m4) 
LRT2 <--2 * L3 + 2 * L4
LRT2 > qchisq(0.95, df = 1)
## Since they are of no difference, we can essentially pick any one of them. 

m5 <- glm.nb(executive_orders~divided+ log(data1$committee_staff_size),data=data1,control = glm.control((maxit=100)))
m6 <- glm.nb(executive_orders~divided+ log(data1$committee_staff_size) + 
           war, data=data1,control= glm.control((maxit=100)))
L5<- logLik(m5)
L6<- logLik(m6) 
LRT3 <--2 * L5 + 2 * L6
LRT3 > qchisq(0.95, df = 1)
## False

m7<- glm.nb(executive_orders~divided+ log(data1$committee_staff_size) + 
            +inflation, data=data1,control= glm.control((maxit=100)))
L7<- logLik(m5)
L8<- logLik(m7) 
LRT3 <--2 * L7 + 2 * L8
LRT3 > qchisq(0.95, df = 1)
## True

m8<- glm.nb(executive_orders~divided+ log(committee_staff_size)  
              +inflation + lame_duck+ administration_change, data=data1,control= glm.control((maxit=100)))
L9<- logLik(m7)
L10<- logLik(m8) 
LRT4 <--2 * L9 + 2 * L10
LRT4 > qchisq(0.95, df = 1)
## False

m9<- glm.nb(executive_orders~divided+ log(data1$committee_staff_size) 
               +inflation + spending_percent_gdp, data=data1,control= glm.control((maxit=100)))
L11<- logLik(m7)
L12<- logLik(m9) 
LRT5 <--2 * L11 + 2 * L12
LRT5 > qchisq(0.95, df = 1)
## True 
m10<- glm.nb(executive_orders~divided+ log(data1$committee_staff_size) + 
            +inflation + spending_percent_gdp+ presidential_expenditures, data=data1,control= glm.control((maxit=100)))
L13<- logLik(m9)
L14<- logLik(m10) 
LRT6 <--2 * L13 + 2 * L14
LRT6 > qchisq(0.95, df = 1)
## True 

m11<- glm.nb(executive_orders~divided+ log(data1$committee_staff_size) + inflation + spending_percent_gdp+presidential_expenditures+ pre_lra, 
            data=data1,control= glm.control((maxit=100)))

L15<- logLik(m10)
L16<- logLik(m11) 
LRT7 <--2 * L15 + 2 * L16
LRT7 > qchisq(0.95, df = 1)
## True. 

## We used m11 as our moel to run our simulations in the later process. 

#### Present Coefficient Tables 
coef(m11)

stargazer(list(m4,m7,m10,m11
),
out="QM_Essay.tex",
title = "Regression Results",
covariate.labels = c(
  "A", 
  "B", 
  "C",
  "D",
  "E",
  "F"
))

# To ensure smooth coding, A,B,C,D,E,F, is placed here. They correspond to A: Divided, B: log Committee staff size 
# C: Inflation, D: Spending percentage GDP, E: Presidential expenditures F: Prior to 1946, respectively. 

## Simulation Process 
m_real<- m11
nsim <- 1000
beta_hat<- coef(m_real)
V_hat <- vcov(m_real)
S <- mvrnorm(nsim, beta_hat, V_hat)

## Interesting scenario 
# 1) Intercept 
# 2) Divided or not 
# 3) Committee_staff_size
# 4) inflation 
# 5) spending_percent_gdp
# 6) presidential_expenditures
# 7) pre_lra

## Scenarios for testing hypothesis 1 
scenario_LowCap_Divided <- cbind(        1,
                                         1,
                                         quantile(log(data1$committee_staff_size),0.25),
                                         mean(data1$inflation),
                                         mean(data1$spending_percent_gdp),
                                         mean(data1$presidential_expenditures),
                                         median(data1$pre_lra))


scenario_LowCap_United <- cbind(1,
                                0,
                                quantile(log(data1$committee_staff_size),0.25),
                                mean(data1$inflation),
                                mean(data1$spending_percent_gdp),
                                mean(data1$presidential_expenditures),
                                median(data1$pre_lra))
########
########

Xbeta_LC_Divided<- S %*% t(scenario_LowCap_Divided)
Xbeta_LC_United <- S%*% t(scenario_LowCap_United) 

lambda_LC_Divided<- exp(Xbeta_LC_Divided)
lambda_LC_United <- exp(Xbeta_LC_United)

theta_m_real <- m_real$theta

exp_scenario_LC_Divided <- sapply(lambda_LC_Divided, 
                                  function(x) mean(rnbinom(1000, size = theta_m_real, mu = x)))
exp_scenario_LC_United <- sapply(lambda_LC_United, 
                                 function(x) mean(rnbinom(1000, size = theta_m_real, mu = x)))
EV<- cbind(exp_scenario_LC_Divided,
           exp_scenario_LC_United)
df1 <- data.frame(EV)
df1

df1$FD <-exp_scenario_LC_Divided- exp_scenario_LC_United
df1$FD
meanFD<- mean(df1$FD)
meanFD
CI_FD1 <- quantile(df1$FD, probs = c(0.025, 0.975))
CI_FD1 
## Table for first difference
Table1 <-  cbind(c(CI_FD1[1], meanFD, CI_FD1[2] ))
titles <-  c("confidence  % 95 low ", "First Difference",
             " Confidence %95 high " )
column <- c("values")
rownames(Table1) <- titles
colnames(Table1) <- column
stargazer(Table1,
          out="QM_Essay_H2_FD_TABLE_1",
          title = "First Difference between divided government and unified government under low legislative capacity" 
)
Table1

##First difference with low legislative capacity 
par(mfrow=c(1, 1))
cis1 <- rbind(CI_FD1)
col <- c("black")

par(mar = c(5, 1, 4, 7) + .1)
plot(
  y = c(1:1),
  x = c(
    meanFD
  ),
  cex = 1.5,
  xlab = 'First Difference and 95%-CI of Expected Executive Orders
     between two scenarios (mean)',
  col = col,
  ylab = '',
  yaxt = "n",
  xlim = c(-15, 10),
  ylim = c(0.5 , 1.5),
  pch = 19,
  main = 'First difference 
  under low legislative capacity (Divided - Unified) ',
  bty = "n"
)
axis(
  4,
  at = c(1:1),
  labels = c("First Difference"),
  las = 2
)
segments(
  x0 = c(cis1[1, 1]),
  y0 = c(1),
  x1 = c(cis1[1, 2]),
  y1 = c(1),
  col = col,
  lwd = 2.3
)
segments(
  x0 = 0,
  y0 = 0,
  x1 = 0,
  y1 = 2,
  lty = "dashed"
)

######
######
## Scenarios created to test hypothesis 2 
scenario_HC_Divided<- cbind(1,
                                 1,
                                 quantile(log(data1$committee_staff_size),0.75),
                                 mean(data1$inflation),
                                 mean(data1$spending_percent_gdp),
                                 mean(data1$presidential_expenditures),
                                 median(data1$pre_lra))

scenario_HC_United<- cbind(     1,
                                0,
                                quantile(log(data1$committee_staff_size),0.75),
                                mean(data1$inflation),
                                mean(data1$spending_percent_gdp),
                                mean(data1$presidential_expenditures),
                                median(data1$pre_lra))

X_beta_HC_Divided <- S %*%t(scenario_HC_Divided)
X_beta_HC_United <- S %*%t(scenario_HC_United)

lambda_HC_Divided<- exp(X_beta_HC_Divided)
lambda_HC_United <- exp(X_beta_HC_United)

exp_scenario_HC_Divided <- sapply(lambda_HC_Divided, function(x) mean(rnbinom(1000, size = theta_m_real, mu = x)))
exp_scenario_HC_United <- sapply(lambda_HC_United, function(x) mean(rnbinom(1000, size = theta_m_real, mu = x)))

EV2<- cbind(exp_scenario_HC_Divided,exp_scenario_HC_United)
df2 <- data.frame(EV2)

df2$FD <-exp_scenario_HC_Divided- exp_scenario_HC_United
df2$FD
meanFD2<- mean(df2$FD)
meanFD2
CI_FD2 <- quantile(df2$FD, probs = c(0.025, 0.975))
CI_FD2

### CI Table for first difference with High Legislative power

Table2 <-  cbind(c(CI_FD2[1], meanFD2, CI_FD2[2] ))
titles <-  c("confidence % 95 low ", "First Difference",
             " confidence %95 high " )
column <- c("values")
rownames(Table4) <- titles
colnames(Table4) <- column
Table4
stargazer(Table4,
          out="QM_Essay_H2_FD",
          title = "First Difference between divided government and united government under high legislative capacity" 
)

###### 
### First difference plot for the second Hypothesis. 
cis2 <- rbind(CI_FD2)
col <- c("black")
par(mfrow=c(1, 1))
par(mar = c(5, 1, 4, 7) + .1)
plot(
  y = c(1:1),
  x = c(
    meanFD2
  ),
  cex = 1.5,
  xlab = 'First Difference and 95%-CI of Expected Executive Orders
     between two scenarios (mean)',
  col = col,
  ylab = '',
  yaxt = "n",
  xlim = c(-10, 10),
  ylim = c(0.5 , 1.5),
  pch = 19,
  main = 'First Differences
  under high legislative capacity (Divided - Unified)',
  bty = "n"
)
axis(
  4,
  at = c(1:1),
  labels = c("First Difference"),
  las = 2
)
segments(
  x0 = c(cis2[1, 1]),
  y0 = c(1),
  x1 = c(cis2[1, 2]),
  y1 = c(1),
  col = col,
  lwd = 2.3
)
segments(
  x0 = 0,
  y0 = 0,
  x1 = 0,
  y1 = 2,
  lty = "dashed"
)

## Figure 4

par(mar=c(5,4,4,4))
par(mfrow=c(1, 2))
plot(data1$year,
     log(data1$committee_staff_size),
     main = "log(committee staff size) from 1905- 2009",
     xlab = "Year",
     ylab =  "logged Committee staff size",
     bty = "n",
     las = 1,
     pch = 1,
     col= "darkgreen"
     )

plot(data1$year,
     log(data1$legislative_expenditures),
     main = "log(legislative expenditures) from 1905- 2009",
     xlab = "Year",
     ylab =  " log legislative expenditures",
     bty = "n",
     las = 1,
     pch = 1,
     col= "blue"
     )
## Figure 5 
par(mfrow=c(1, 1))
plot(data1$year, 
     data1$executive_orders,
     main = "Executive Orders issued from 1905- 2009",
     xlab = "Year",
     ylab =  "Frequency",
     bty = "n",
     las = 1,
     pch = 1,
     col= "black"
)

Table1 <-  cbind(c(CI_FD1[1], meanFD2, CI_FD1[2] ))
titles <-  c("confidence % 95 low ", "First Difference",
             " confidence %95 high " )
column <- c("values")
rownames(Table1) <- titles
colnames(Table1) <- column

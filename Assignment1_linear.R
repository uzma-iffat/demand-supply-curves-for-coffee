getwd()
setwd("C:/Analytics in Manegerial Economics")

data = read.csv(file="Project1Dataset.csv",header=TRUE)
str(data)
?lm
plot(data$qu,data$cprice)

#Looking at data quarter wise 
data_q1 <- subset(data,q1==1)
data_q2 <- subset(data,q2==1)

plot(data_q1$qu,data_q1$cprice)
plot(data_q2$qu,data_q2$cprice)
plot(data$cprice,data$q1)

#Reduced form model for Demand
rdf_demand = lm(qu ~ tprice + oprice + incom + q1 + q2 + q3 + q4 + bprice + wprice,data)
summary(rdf_demand)

#Some initial analysis suggests that a log plot is more likely 

data$ln_qu=log(data$qu)
data$ln_cprice=log(data$cprice)
data$ln_tprice=log(data$tprice)
data$ln_wprice=log(data$wprice)
data$ln_bprice=log(data$bprice)
data$ln_incom=log(data$incom)

plot(data$ln_qu,data$ln_cprice)

######## Linear approach i.e. non constant elasticity #######
#Step 1 : Reduced form equations for cprice and qu  

data$sqincom = data$incom ^ 2
data$sqwprice = data$wprice ^ 2
data$sqbprice = data$bprice ^ 2
data$sqtprice = data$tprice ^ 2

rdf_cprice = lm( cprice ~ sqtprice + sqincom + q1 + q2 + q3 + q4 + sqbprice + sqwprice, data)
summary(rdf_cprice)

rdf_qu = lm(qu ~ sqtprice + sqincom + q1 + q2 + q3 + q4 + sqbprice + sqwprice,data)
summary(rdf_qu)  ### bad R squared ... dataset doesnt have enough variables to explain quantity

#Step 2 : Run OLS assuming no endogeniety 

##Demand Curve 
data$r_cprice = data$cprice/data$oprice
data$r_incom = data$incom/data$oprice
data$r_tprice = data$tprice/data$oprice

ols_demand = lm(qu ~ r_cprice + r_incom + r_tprice,data)
summary(ols_demand)

##Supply Curve
data$r_bprice = data$bprice/data$oprice
data$r_wprice = data$wprice/data$oprice

ols_supply = lm(qu ~ r_cprice + r_bprice + r_wprice + q1 + q2 + q3 + q4, data)
summary(ols_supply)

############ Step 3 : 2SLS ############3

### for demand curve
### Instrument for r_cprice as r_bprice

cprice_1 = lm(r_cprice ~ r_bprice,data)
summary(cprice_1)

tsls_demand =  ivreg(qu ~ r_cprice + r_incom + r_tprice, ~ r_bprice + r_incom + r_tprice,data)
summary(tsls_demand)

### for supply curve
### Instrument for r_cprice be r_incom and r_tprice

cprice_2 = lm(r_cprice ~ r_incom + r_tprice,data)
summary(cprice_2)

tsls_supply = ivreg(qu ~ r_cprice + q1 +q2 + q3 + r_bprice + r_wprice, ~r_incom + r_tprice +q1 +q2 + q3 + r_bprice + r_wprice,data)
summary(tsls_supply)
summary(cprice_1)

install.packages("AER")
library("AER")

tsls_demand =  ivreg(qu ~ r_cprice + r_incom + r_tprice | r_cprice + r_incom + r_tprice,data)

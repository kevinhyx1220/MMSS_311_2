# Homework 0, April 14
# Kevin He, MMSS311-2

setwd("~/GitHub/MMSS_311_2")

# Question 1

# 1a
x1a <- c(1, 2, 3, 4, 5)
print(x1a)

# 1b
Mindy <- 12
print(Mindy)

# 1c
x1c <- matrix(1:6, nrow = 2, byrow = TRUE)
print(x1c)

# 1d
x1d <- matrix(1:6, nrow = 2, byrow = FALSE)
print(x1d)

# 1e
x1e <- matrix(1, nrow = 10, ncol = 10)
print(x1e)

# 1f
x1f <- c("THIS", "IS", "A", "VECTOR")
print(x1f)

# 1g
f1g <- function(x1g1, x1g2, x1g3){
  sum1g <- x1g1 + x1g2 + x1g3
  return(sum1g)
}

# 1h
f1h <- function(x1h){
  if(x1h <= 10){
    return("Yes")
  } 
  else {
    return("No")
  }
}

# 1i
g <- rnorm(n = 1000, mean = 10, sd = 1)

# 1j
y <- rnorm(n = 1000, mean = 5, sd = 0.5)

# 1k
f1khelper <- function(){
  f1k2 <- sample(g, 10, replace = TRUE, prob = NULL)
  f1k2mean <- mean(f1k2)
  return(f1k2mean)
}
x <- vector()
for (i in 1:1000) {
  x[i] = f1khelper()
}
  
# 1l
x1l <- lm(y ~ x)
summary(x1l)

# Question 2

# 2a
pums <- read.csv("pums_chicago.csv",header = TRUE)

# 2b
length(colnames(pums))
# 204 variables

# 2c
mean(pums$PINCP[!is.na(pums$PINCP)])
# 38247.62 

# 2d
pums$PINCP_LOG <- log10(pums$PINCP)
# Yes, NaN's were produced, because there were NA's in the original PINCP data

# 2e
pums$GRAD.DUMMY <- pums$SCHL > 17 
pums$GRAD.DUMMY[pums$GRAD.DUMMY == TRUE] <- "grad"
pums$GRAD.DUMMY[pums$GRAD.DUMMY == FALSE] <- "no grad"

# 2f
pums$SERIALNO <- NULL

# 2g
write.csv(pums, file = "pumsedited.csv", row.names = FALSE) 

# 2h
under_16 <- pums[is.na(pums$ESR),]
employed <- pums[pums$ESR %in% c(1,2),]
unemployed <- pums[pums$ESR == 3,]
in_the_armed_forces <- pums[pums$ESR %in% c(4,5),]
not_in_labor_force <- pums[pums$ESR == 6,]

# 2i
employed_af <- rbind(employed, in_the_armed_forces)

# 2j
employed_af <- subset(employed_af, , select = c(AGEP, RAC1P, PINCP_LOG))

# 2k(i)
mean(pums$JWMNP[!is.na(pums$JWMNP)])
median(pums$JWMNP[!is.na(pums$JWMNP)])
quantile(pums$JWMNP[!is.na(pums$JWMNP)], 0.8)
# mean is 34.84
# median is 30
# 80% percentile is 45

# 2k(ii)
cor(pums$JWMNP[!is.na(pums$JWMNP) & !is.na(pums$WAGP)] , 
    pums$WAGP[!is.na(pums$JWMNP) & !is.na(pums$WAGP)])
# correlation between JWMNP and WAGP is -0.04205232

# 2k(iii)
plot(pums$AGEP, pums$PINCP_LOG)

# 2k(iv)
pdf(file = "2kiv_scatterplot.pdf")
plot(pums$AGEP, pums$PINCP_LOG)
dev.off()

# 2k(v)
table2kv <- xtabs(~pums$ESR + pums$RAC1P)
print(table2kv)

# 2k(vi)
x2kvi <- lm(pums$WAGP ~ pums$WKHP)
summary(x2kvi)

# 2k(vii)
x2kvi_residual = resid(x2kvi)
x2kvi_fitted = fitted(x2kvi)
plot(x2kvi_fitted, x2kvi_residual)
"This plot shows that the residuals are relatively symmetrically distributed
across zero, indicating that the linear model works decently well."

# 2l(i)
data(mtcars)
x2li <- lm(mtcars$mpg ~ mtcars$wt)
summary(x2li)

# 2l(ii)
# Manual transmission
x2lii_manual <- lm(mtcars$mpg[mtcars$am == 1] ~ mtcars$wt[mtcars$am == 1])
summary(x2lii_manual) 
# Automatic transmission
x2lii_auto <- lm(mtcars$mpg[mtcars$am == 0] ~ mtcars$wt[mtcars$am == 0])
summary(x2lii_auto)

# 2l(iii)
mtcars_horsepower <- log10(mtcars$hp)
x2liii <- lm(mtcars$mpg ~ mtcars_horsepower)
summary(x2liii)

# 2m(i-v)
library("ggplot2")
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(aes(col=am, shape=gear)) +
  scale_shape_identity() + 
  labs(x="Weight", y="Miles per gallon") + 
  theme(panel.background = element_rect(fill='pink')) 



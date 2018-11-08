rt<- read.table("E:/Dokumente und Einstellungen/hp/Eigene Dateien/LAItest.txt")
rt
curve(x+0,0,100)
points(rt$V3~rt$V2)
lm.sol<-lm(V3~V2, data=rt)
abline(lm.sol)
summary(lm.sol)

Call:
lm(formula = V3 ~ V2, data = rt)


Residuals:
      1       2       3       4       5       6 
  9.575  10.576  17.493 -22.398  -7.751  -7.496 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.09226   16.09559  -0.006   0.9957  
V2           1.43690    0.31729   4.529   0.0106 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 16.79 on 4 degrees of freedom
Multiple R-Squared: 0.8368,     Adjusted R-squared: 0.796 
F-statistic: 20.51 on 1 and 4 DF,  p-value: 0.01059 


par(mfrow=c(5,3))
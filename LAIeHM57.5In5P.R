rt<- read.table("E:/Dokumente und Einstellungen/hp/Eigene Dateien/Stn3/Stn_03002HM5P.txt")
rt

x=rt$V1
y=-log(rt$V2)*cos(x*pi/180)
lm.sol<-lm(y~x)
summary(lm.sol)

LAIe=-log(rt$V2[6])*cos(rt$V1[6]*pi/180)*2
LAIe

G=y/LAIe
G

Theta=x*pi/180
Theta

plot(G~Theta,ylim=c(0,1))
lm.sol<-lm(G~Theta)
abline(lm.sol)

ellipsoid.distribution <- deriv(~ sqrt(a^2 * cos(theta)^2 + sin(theta)^2)/(a+1.774*(a+1.182)^-0.733), c("a"), function(a, theta) {})
ellipse.nls <- nls(G ~ ellipsoid.distribution(a, Theta), start = c(a = 2))

summary(ellipse.nls)

a=2
curve( sqrt(a^2 * cos(x)^2 + sin(x)^2)/(a+1.774*(a+1.182)^-0.733),0,pi/2,add=TRUE)


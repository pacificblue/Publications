#Ellipse fitting program
#Before running, put points data in "c:/E_points.txt" and powers data in "c:/E_powerpairs.txt"

# clear memory
rm(list=ls(all=TRUE)) 

Pointsfile="c:/E_points.txt"

#read in data
points<-read.table(Pointsfile)  # read in points data (x,y)
points


powerpair<-read.table("c:/E_powerpairs.txt", header=TRUE) # read in powers (x1, y1, x2, y2....)



#seperate powers into two matrices of x and y  like (x1, x2...) and (y1, y2....)
xpower=cbind(powerpair$px1,powerpair$px2,powerpair$px3,powerpair$px4,powerpair$px5,powerpair$px6)
ypower=cbind(powerpair$py1,powerpair$py2,powerpair$py3,powerpair$py4,powerpair$py5,powerpair$py6)

#initiate coefficients matrix  
coes<-matrix(0,nrow=6,ncol=6)

#get fitting points number
pointsnumber<-length(attributes(points)$row.names)


#compute coes matrix
for(i in 1:pointsnumber)
{
 x=points[i,1]^xpower  #apply powers on X
 y=points[i,2]^ypower  #apply powers on Y
 coes=coes+x*y         #sigma 
}

coes

L<-chol(coes)
InvL=solve(L)
InvL_t<-t(InvL)

Const<-matrix(0,nrow=6,ncol=6)
Const[1,3]<--2
Const[2,2]<-1
Const[3,1]<--2

Const

dmy<-Const%*%InvL_t
C<-InvL%*%dmy

eigenList<-eigen(C, symmetric=TRUE)
d<-eigenList$values
V<-eigenList$vectors
d
V


sol<-InvL_t%*%V
sol
norm<-0
for(i in 1:6)
{
 for(j in 1:6)
 {
  norm<-norm+sol[i,j]*sol[i,j]
 }
}

for(i in 1:6)
{
 for(j in 1:6)
 {
  sol[i,j]<-sol[i,j]/sqrt(norm)
 }
}


for(i in 1:6)
{
 if(d[i]<0 & abs(d[i])>0) 
 solind<-i
}
  
for(j in 1:6)
{d[j]=sol[j,solind]}

d



points<-read.table(Pointsfile)  # read in points data (x,y)
points

pointsnumber<-length(attributes(points)$row.names)

plot(points)
curve( (((2*d[2]*x+d[5])^2-4*d[3]*(d[1]*x^2+d[4]*x+d[6]))^0.5-(2*d[2]*x+d[5]))/(2*d[3]), min(points$V1), max(points$V1),add=TRUE)
curve( (-((2*d[2]*x+d[5])^2-4*d[3]*(d[1]*x^2+d[4]*x+d[6]))^0.5-(2*d[2]*x+d[5]))/(2*d[3]),min(points$V1), max(points$V1),add=TRUE)




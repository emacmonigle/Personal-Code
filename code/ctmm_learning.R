#onboarding to addax movement of life prject
#december 21st
#erin macmonigle


#install.packages("ctmm")

library(ctmm)

#import and configure data
data("buffalo")

Cilla <- buffalo$Cilla

Gabs <- buffalo$Gabs

#all movment points in one color
plot(Cilla)

title("1 buffalo")

#all movment points by species (?)
plot(buffalo, col=rainbow(length(buffalo)))

title("5 buffalo")

#creating a variogram
SVF <- variogram(Cilla) #create variogram df with Cilla

level <- c(0.5, 0.95)
xlim <- c(0,12 %#% "hour")


plot(SVF, xlim = xlim, level = level)

title("Zoomed In")

plot(SVF, fraction = 0.65, level = level)

title("zoomed out")  


zoom(Cilla)

#manual variogram
m.iid <- ctmm(sigma=23 %#% "km^2")
m.ou <- ctmm(sigma=23 %#% "km^2", tau=6 %#% "day")


plot(SVF, CTMM = m.iid, fraction = 0.65, level = level, col.CTMM = "red")
title("Independent and identically distributed data")

plot(SVF, CTMM = m.ou, fraction = 0.65, level = level, col.CTMM = "purple")
title("Ornstein-Uhlenbeck movement")

m.ouf <- ctmm(sigma=23 %#% "km^2",tau=c(6 %#% "day",1 %#% "hour"))

plot(SVF, CTMM = m.ou, level = level, col.CTMM = "purple",  xlim = xlim)
title("Ornstein-Uhlenbeck movement")

plot(SVF,CTMM=m.ouf,level=level,col.CTMM="blue",xlim=xlim)
title("Ornstein-Uhlenbeck-F movement")

plot(SVF,CTMM=m.ou,fraction=0.65,level=level,col.CTMM="purple")
title("Ornstein-Uhlenbeck movement")

plot(SVF,CTMM=m.ouf,fraction=0.65,level=level,col.CTMM="blue")
title("Ornstein-Uhlenbeck-F movement")  


#simulate fake buffalo sampling schedule
willa <- simulate(m.ouf, t = Cilla$t)

plot(willa)
title("simulation")

SVF2 <- variogram(willa)
plot(SVF2,CTMM=m.ouf,fraction=0.65,level=level,col.CTMM="blue")
title("simulation")


#modeling sampling schedules
data("gazelle")

dt.plot(gazelle)
title("sampling intervals")

SVF3 <- variogram(gazelle[[18]])
plot(SVF3, fraction = 0.85, level = level)
title("default method")

dt <- c(1,5,25) %#% "hour"
SVF3 <- variogram(gazelle[[18]],dt=dt)
plot(SVF3,fraction=0.85,level=level)
title("multi method")

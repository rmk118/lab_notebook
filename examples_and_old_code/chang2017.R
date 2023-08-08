#Examples/practice from Empirical Dynamic Modeling for Beginners
# (Chang et al., 2017 ) https://link.springer.com/article/10.1007/s11284-017-1469-9 
#Ruby Krasnow
#6/23/23


# time series of red noise and logistic map - my way --------------------------------------------------------------------
dat2 <- read.csv('ESM2_Data_noise.csv',header=T)
dat2 <- dat2 %>% mutate(index=c(1:1000), .before=R)

# Data normalization
dat2 <- dat2 %>% mutate(
  Red2 = (R-mean(R))/sd(R),
  Logi2 = (L-mean(L))/sd(L), .keep="unused"
)

red_time_series<-ggplot(dat2, aes(x=index, y=Red2))+geom_line()+theme_classic()+ylab("Standardized density")+xlab("Time")+ggtitle("Red noise") #mine
red_time_series #plot time series

logi_time_series<-ggplot(dat2, aes(x=index, y=Logi2))+geom_line()+theme_classic()+ylim(-2, 2)+ylab("")+xlab("Time")+ggtitle("Logistic map") #mine
logi_time_series #plot time series

rho_E2 <- EmbedDimension(dataFrame = dat2, columns = "Red2", target = "Red2",lib = "1 500", pred = "501 1000", maxE=8, showPlot = FALSE)
optimalRhoRed<-(as.ggplot(~plot(rho_E2, type="l", ylab=expression(rho), xlab="E")))
optimalRhoRed
rho_E2[which.max(rho_E2$rho), "E"] #7 for red

rho_E3 <- EmbedDimension(dataFrame = dat2, columns = "Logi2", target = "Logi2",lib = "1 500", pred = "501 1000", maxE=8, showPlot = TRUE)
optimalRhoLogi<-as.ggplot(~plot(rho_E3, ylim=c(0.95,1.02), type="l", ylab=""))
rho_E3[which.max(rho_E3$rho), "E"] #1 for logi

#test for nonlinearity
rho_thetaR = PredictNonlinear(dataFrame = dat2, columns = "Red2",
                              target = "Red2", lib = "1 500", pred = "501 1000", theta=seq(0,2,0.1), E = 7)
rho_vs_thetaR<-as.ggplot(~plot(rho_thetaR, type="l",ylim=c(0.4,0.5), ylab=expression(rho), xlab=expression(theta)))

rho_thetaL = PredictNonlinear(dataFrame = dat2, columns = "Logi2",
                              target = "Logi2", lib = "1 500", pred = "501 1000", theta=seq(0,2,0.1), E = 2)
rho_vs_thetaL<-as.ggplot(~plot(rho_thetaL, type="l",ylim=c(0.6,1), ylab="",xlab=expression(theta)))

grid.arrange(red_time_series, logi_time_series, optimalRhoRed, optimalRhoLogi, rho_vs_thetaR, rho_vs_thetaL, ncol=2) #SUCCESS

allGraphs<- (red_time_series / optimalRhoRed / rho_vs_thetaR) | (logi_time_series / optimalRhoLogi / rho_vs_thetaL)
allGraphs

#time series of Moran effect model - my way --------------------------------------------------------------------
# Loading the time series for the Moran effect and mirage correlation models
dam2.pre <- read.csv('ESM3_Data_moran.csv',header=T) # Moran effect

dam2 <- scale(dam2[,-1], center = TRUE, scale = TRUE)# Data normalization

damShort<- dam2 %>% filter(Time<101)

#plot time series
moran_series<-ggplot(damShort, aes(x=Time))+
  geom_line(aes(y = N1, color = "N1"))+ theme_classic()+
  geom_line(aes(y = N2, color = "N2"))+
  ylab(expression("Density (capita*m" ^"-3"~")"))+xlab("Time")+ggtitle("Moran effect")+
  scale_color_manual(values=c("Black", "Red"))+ theme(legend.title = element_blank())
moran_series

#Determine optimal embedding dimension
rho_N1 <- EmbedDimension(dataFrame = dam2, columns = "N2", target = "N1",lib = "1 500", pred = "501 1000", maxE=8, showPlot = TRUE)
rho_N1[which.max(rho_N1$rho), "E"] #5 for N1

rho_N2 <- EmbedDimension(dataFrame = dam2, columns = "N1", target = "N2",lib = "1 500", pred = "501 1000", maxE=8, showPlot = TRUE)
rho_N2[which.max(rho_N2$rho), "E"] #6 for N2, but 5 is almost as good

# CCM analysis of the Moran effect model, N1 and N2
CCM(dataFrame = dam2, E = 5, Tp = 0, columns = "N1",
    target = "N2", libSizes = "100 1000 200", sample = 100, showPlot = TRUE)


# # time series of red noise and logistic map - error way --------------------------------------------------------------------
# dat <- read.csv('ESM2_Data_noise.csv',header=T)
#
# # Data normalization
# Red <- ((dat[,"R"]-mean(dat[,"R"]))/sd(dat[,"R"]))
# Logi <- ((dat[,"L"]-mean(dat[,"L"]))/sd(dat[,"L"]))
#
# plot(Red, type="l") #plot time series
#
# # Simplex projection for red noise and logistic map
# sim_r <- simplex(Red,lib=c(1,500),pred=c(501,1000),E=c(2:8))
# sim_l <- simplex(Logi,lib=c(1,500),pred=c(501,1000),E=c(2:8))
#
# ## Plot predictive skill (rho) vs embedding dimension (E)
# par(mfrow=c(2,1),mar=c(4,4,1,1))
# plot(rho~E,data=sim_r,type="l",xlab="Embedding dimension (E)",ylab=expression(rho),ylim=c(0.3,0.4),col=2,main="Red noise")
# plot(rho~E,data=sim_l,type="l",xlab="Embedding dimension (E)",ylab=expression(rho),ylim=c(0.95,1.02),col=4,main="Logistic map")
#
# #S map for Red Noise & logistic map
# smap_r <- s_map(Red,E=7,lib=c(1,500),pred=c(501,1000),theta=seq(0,2,0.1))
# smap_l <- s_map(Logi,E=2,lib=c(1,500),pred=c(501,1000),theta=seq(0,2,0.1))
#
# # Plot predictive skill (rho) vs state-dependency parameter (theta)
# plot(rho~theta,data=smap_r,type="l",xlab=expression(theta),ylab=expression(rho),ylim=c(0.4,0.5),col=2,main="Red noise")
# plot(rho~theta,data=smap_l,type="l",xlab=expression(theta),ylab=expression(rho),ylim=c(0.6,1),col=4,main="Logistic map")
#
# # The optimal theta determined by maximizing rho
# (the_r <- smap_r[which.max(smap_r$rho),"theta"][1])
# (the_l <- smap_l[which.max(smap_l$rho),"theta"][1])

#time series of Moran effect model - my way --------------------------------------------------------------------
# Loading the time series for the Moran effect and mirage correlation models
dam2.pre <- read.csv('ESM3_Data_moran.csv',header=T) # Moran effect

dam2 <- scale(dam2.pre[,-1], center = TRUE, scale = TRUE)# Data normalization

# Data normalization
dam3 <- dam2.pre %>% mutate(
  N1 = (N1-mean(N1))/sd(N1),
  R1 = (R1-mean(R1))/sd(R1),
  R2 = (R2-mean(R2))/sd(R2),
  N2 = (N2-mean(N2))/sd(N2)
)

damShort<- dam3 %>% filter(Time<101)

#plot time series
moran_series<-ggplot(damShort, aes(x=Time))+
  geom_line(aes(y = N1, color = "N1"))+ theme_classic()+
  geom_line(aes(y = N2, color = "N2"))+
  ylab(expression("Density (capita*m" ^"-3"~")"))+xlab("Time")+ggtitle("Moran effect")+
  scale_color_manual(values=c("Black", "Red"))+ theme(legend.title = element_blank())
moran_series

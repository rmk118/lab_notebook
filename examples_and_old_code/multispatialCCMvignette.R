#Multispatial CCM package vignette
#Ruby Krasnow
#Last modified: 6/26/2023

library(multispatialCCM)
library(patchwork)


# Example data ------------------------------------------------------------

#Simulate data to use for multispatial CCM test
#See function for details - A is causally forced by B,
#but the reverse is not true.
ccm_data_out<-make_ccm_data()

Accm<-ccm_data_out$Accm
Bccm<-ccm_data_out$Bccm
#Calculate optimal E
maxE<-5 #Maximum E to test
#Matrix for storing output
Emat<-matrix(nrow=maxE-1, ncol=2); colnames(Emat)<-c("A", "B")

#Loop over potential E values and calculate predictive ability
#of each process for its own dynamics
for(E in 2:maxE) {
  #Uses defaults of looking forward one prediction step (predstep)
  #And using time lag intervals of one time step (tau)
  Emat[E-1,"A"]<-SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1,"B"]<-SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

#Look at plots to find E for each process at which
#predictive ability rho is maximized
matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2,
        xlab="E", ylab="rho", lwd=2)
legend("bottomleft", c("A", "B"), lty=1:2, col=1:2, lwd=2, bty="n")

E_A<-2
E_B<-3

#Check data for nonlinear signal that is not dominated by noise
#Checks whether predictive ability of processes declines with
#increasing time distance
#See manuscript and R code for details
signal_A_out<-SSR_check_signal(A=Accm, E=E_A, tau=1,
                               predsteplist=1:10)
signal_B_out<-SSR_check_signal(A=Bccm, E=E_B, tau=1,
                               predsteplist=1:10)

plot(signal_A_out$predatout)

#Run the CCM test
#E_A and E_B are the embedding dimensions for A and B.
#tau is the length of time steps used (default is 1)
#iterations is the number of bootstrap iterations (default 100)
# Does A "cause" B?
#Note - increase iterations to 100 for consistent results
CCM_boot_A<-CCM_boot(Accm, Bccm, E_A, tau=1, iterations=10)
# Does B "cause" A?
CCM_boot_B<-CCM_boot(Bccm, Accm, E_B, tau=1, iterations=10)

#Test for significant causal signal
#See R function for details
(CCM_significance_test<-ccmtest(CCM_boot_A,
                                CCM_boot_B))

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
#Plot "A causes B"
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2)
legend("topleft",
       c("A causes B", "B causes A"),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n")


# Nitrogen E001 unfertilized -----------------------------------------------------------

e0010<- read.csv("~/Downloads/lab_notebook/example_code/multispatialCCMexamples/e001_arssnlvl0.csv")

e0010test<- e0010 %>% 
  group_by(FieldPlot) %>% 
  summarise(years=n_distinct(Year))

#Remove the isolated 2008 years and the extra NA rows that result
e0010 <- e0010 %>% 
  filter(is.na(Year)|Year != 2008)
for (i in 1:(nrow(e0010)-1)) {
  if (!is.na(e0010[i,"Year"]) | !is.na(e0010[i+1,"Year"])) {
    e0010<-e0010
  }
  else {
    e0010<-e0010 %>%slice(-i)
  }
}

repens <- e0010$Agropyron.repens
scop <- e0010$Schizachyrium.scoparium

#Calculate optimal E
maxE2<-20 #Maximum E to test
#Matrix for storing output
Emat2<-matrix(nrow=maxE2-1, ncol=2); colnames(Emat2)<-c("A. repens", "S. scoparium")

for(E in 2:maxE2) {
  #Uses defaults of looking forward one prediction step (predstep)
  #And using time lag intervals of one time step (tau)
  Emat2[E-1,"A. repens"]<-SSR_pred_boot(A=repens, E=E, predstep=1, tau=1)$rho
  Emat2[E-1,"S. scoparium"]<-SSR_pred_boot(A=scop, E=E, predstep=1, tau=1)$rho
}

#predictive skill by embedding dimension
matplot(2:maxE2, Emat2, type="l", col=1:2, lty=1:2,
        xlab="E", ylab="rho", lwd=2)
legend("bottomleft", c("A. repens", "S. scoparium"), lty=1:2, col=1:2, lwd=2, bty="n")

E_repens<-5
E_scop<-16

#Check data for nonlinear signal that is not dominated by noise
#Checks whether predictive ability of processes declines with increasing time distance
signal_rep_out<-SSR_check_signal(A=repens, E=E_repens, tau=1,
                               predsteplist=1:25)
signal_scop_out<-SSR_check_signal(A=scop, E=E_scop, tau=1,
                               predsteplist=1:25)

plot(signal_rep_out$predatout, type="l")
plot(signal_scop_out$predatout, type="l")

# Nitrogen E001 fertilized -----------------------------------------------------------
e0013<- read.csv("~/Downloads/lab_notebook/example_code/multispatialCCMexamples/e001_arssnlvl3.csv")

e0013test<- e0013 %>% 
  group_by(FieldPlot) %>% 
  summarise(years=n_distinct(Year))

e0013 <- e0013 %>% 
  filter(is.na(Year)|Year != 2008)
for (i in 1:(nrow(e0013)-1)) {
  if (!is.na(e0013[i,"Year"]) | !is.na(e0013[i+1,"Year"])) {
    e0013<-e0013
  }
  else {
    e0013<-e0013 %>%slice(-i)
  }
}

repens3 <- e0013$Agropyron.repens
scop3 <- e0013$Schizachyrium.scoparium

#Calculate optimal E
maxE3<-20 #Maximum E to test
#Matrix for storing output
Emat3<-matrix(nrow=maxE3-1, ncol=2); colnames(Emat3)<-c("A. repens", "S. scoparium")

for(E in 2:maxE3) {
  #Uses defaults of looking forward one prediction step (predstep)
  #And using time lag intervals of one time step (tau)
  Emat3[E-1,"A. repens"]<-SSR_pred_boot(A=repens3, E=E, predstep=1, tau=1)$rho
  Emat3[E-1,"S. scoparium"]<-SSR_pred_boot(A=scop3, E=E, predstep=1, tau=1)$rho
}

#predictive skill by embedding dimension
matplot(2:maxE3, Emat3, type="l", col=1:2, lty=1:2,
        xlab="E", ylab="rho", lwd=2)
legend("bottomleft", c("A. repens", "S. scoparium"), lty=1:2, col=1:2, lwd=2, bty="n")

E_repens3<-12
E_scop3<-3

#Check data for nonlinear signal that is not dominated by noise
#Checks whether predictive ability of processes declines with increasing time distance
signal_rep3_out<-SSR_check_signal(A=repens, E=E_repens3, tau=1,
                                 predsteplist=1:25)
signal_scop3_out<-SSR_check_signal(A=scop, E=E_scop3, tau=1,
                                  predsteplist=1:25)

plot(signal_rep3_out$predatout, type="l")
plot(signal_scop_out$predatout, type="l")

#Creating time-lagged plot in 2 dimensions (i.e. A. repens population this year, vs. next year)
lagRepens <- e0013 %>%
  mutate(lagR = 0) %>% 
  group_by(FieldPlot) %>% 
  select(Year, FieldPlot, Agropyron.repens, lagR) %>% 
  na.omit()

data_mod <- lagRepens %>%                           
  group_by(FieldPlot) %>%
  dplyr::mutate(laggedval = lag(Agropyron.repens, n = 1, default = NA))

plotA<- ggplot(data_mod, aes(x=Agropyron.repens, y=laggedval, color=Year))+geom_point()+ scale_color_gradientn(colours = rainbow(30))+theme_classic()+labs(x="t0", y="t1")

#Creating time-lagged plot in 2 dimensions (i.e. S. scoparium population this year, vs. next year)
lagScop <- e0013 %>%
  mutate(lagS = 0) %>% 
  group_by(FieldPlot) %>% 
  select(Year, FieldPlot, Schizachyrium.scoparium, lagS) %>% 
  na.omit()

data_modS <- lagScop %>%                           
  group_by(FieldPlot) %>%
  dplyr::mutate(laggedvalS = lag(Schizachyrium.scoparium, n = 1, default = NA))

plotS<- ggplot(data_modS, aes(x=Schizachyrium.scoparium, y=laggedvalS, color=Year))+geom_point()+ scale_color_gradientn(colours = rainbow(30))+theme_classic()+labs(x="t0", y="t1")

#Recreated figure A6!
plotA+plotS +  plot_layout(guides = 'collect')

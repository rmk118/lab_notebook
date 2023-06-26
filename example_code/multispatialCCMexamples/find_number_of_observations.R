getlengths<-function(dat, yearname="Year") {
  tmp<-dat[,yearname]
  return(table(diff(c(0, which(is.na(tmp))))-1))
}



dat<-read.csv("e001_arssnlvl0.csv")
dat$Year[(is.na(dat$Agropyron.repens)|is.na(dat$Schizachyrium.scoparium))]<-NA
getlengths(dat)


dat<-read.csv("e026_aslit.csv")
dat<-dat[dat$nlevel==3,]
dat$Year[(is.na(dat$abvbiomass)|is.na(dat$litbiomass))]<-NA
getlengths(dat, "year")

dat<-read.csv("e026_aslit.csv")
dat<-dat[dat$nlevel==4,]
dat$Year[(is.na(dat$abvbiomass)|is.na(dat$litbiomass))]<-NA
getlengths(dat, "year")


dat<-read.csv("e054_arssprecip.csv")
dat$Year[(is.na(dat$Agropyron.repens)|is.na(dat$precipmm))]<-NA
getlengths(dat)



dat<-read.csv("e120_nitbm.csv")
dat$Year[(is.na(dat$noh020tot)|is.na(dat$AbvBioAnnProd))]<-NA
getlengths(dat)

dat<-read.csv("e120_bmins.csv")
dat$Year[(is.na(dat$insectcount)|is.na(dat$insectsp))]<-NA
getlengths(dat)

dat<-read.csv("e120_invnit1_2.csv")
dat$Year[(is.na(dat$invrichness)|is.na(dat$noh020tot))]<-NA
getlengths(dat)

dat<-read.csv("e120_invnit4_8.csv")
dat$Year[(is.na(dat$invrichness)|is.na(dat$noh020tot))]<-NA
getlengths(dat)

dat<-read.csv("e120_invnit16.csv")
dat$Year[(is.na(dat$invrichness)|is.na(dat$noh020tot))]<-NA
getlengths(dat)





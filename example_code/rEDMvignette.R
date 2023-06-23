#Examples/practice from the rEDM vignette
#Ruby Krasnow
#6/23/23

library(rEDM)

# TentMap example from rEDM vignette (univariate) -----------------------------------------------------------------
# http://127.0.0.1:22304/library/rEDM/doc/rEDM-tutorial.pdf
str(TentMap)
plot(TentMap)
ggplot(TentMap[c(1:500),], aes(x=Time, y=TentMap))+geom_line()

# Simplex projection
simplex_out <- Simplex(dataFrame = TentMap, lib = "1 100", pred = "201 500", columns = "TentMap", target = "TentMap", E = 3, showPlot = TRUE)
simplex_out[c(1:2, 300:301),]
ComputeError(simplex_out$Observations, simplex_out$Predictions) #stats for Simplex projection with E=3

tentMapPredict <- Simplex(dataFrame = TentMap, lib = "1 100", pred = "201 500", columns = "TentMap", target = "TentMap", E = 2, showPlot = TRUE)
ComputeError(tentMapPredict$Observations, tentMapPredict$Predictions)$rho

#Determines optimal embedding dimension by calling simplex fn with E values from 1 to 10 and determining max. rho value
rho_E <- EmbedDimension(dataFrame = TentMap, lib = "1 100", pred = "201 500", columns = "TentMap",target = "TentMap")
rho_E[which.max(rho_E$rho),"E"][1] #2

rho_Tp <- PredictInterval(dataFrame = TentMap, lib = "1 100", pred = "201 500", target = "TentMap", 
                          columns = "TentMap", E = 2) 
#forecasting skill declines as time interval increases, indicates the system may be chaotic

rho_theta <- PredictNonlinear(dataFrame = TentMapNoise, lib = "1 100", pred = "201 500",
                              target = "TentMap", columns = "TentMap", E = 2)
rho_theta[which.max(rho_theta$rho), "Theta"] #optimal theta = 3
#evaluates S-map forecasting skill as theta increases. Since rho isn't maximized at theta=0 and increases as theta increases, we have evidence of nonlinear deterministic behavior. We also see rho decline at high values of theta as the local linear map overfits to insufficient nearest neighbors

#You can also call the SMap function directly, using the values for E and theta found above:
smapTent<- SMap(dataFrame = TentMapNoise, lib = "1 100", pred = "201 500", target = "TentMap", columns = "TentMap", E = 2, theta=3, showPlot = TRUE)
smapTent
head(cbind(smapTent$predictions, smapTent$coefficients), 2)

s_mapTent<- s_map(TentMapNoise, E=2, lib = "1 100", pred = "201 500", theta=3, save_smap_coefficients = TRUE)
s_mapTent


# rEDM vignette - multivariate simplex & S-maps  --------------------------------------------------------------
# http://127.0.0.1:22304/library/rEDM/doc/rEDM-tutorial.pdf

head(block_3sp)
smplx_3species = Simplex(dataFrame = block_3sp, lib = "1 100", pred = "101 190",
                         E = 3, columns = "x_t x_t-1 z_t", target = "x_t", embedded = TRUE)
err = ComputeError(smplx_3species$Observations, smplx_3species$Predictions)

plot(smplx_3species$Observations, smplx_3species$Predictions, pch = 19, cex = 0.5,
     xlab = "Observations", ylab = "Predictions", main = "3 Species x_t")
abline(a = 0, b = 1, lty = 2, col = "blue")
text(-1, 1, paste(capture.output(cbind(err)), collapse = "\n"))

head(Lorenz5D)
str(Lorenz5D)

head(cbind(smap_Lorenz$predictions, smap_Lorenz$coefficients[, 2:6]), 3)

EmbedDimension(dataFrame = Lorenz5D, lib = "1 500", pred = "601 900", columns = "V1", target = "V1", embedded = FALSE, showPlot = TRUE)
EmbedDimension(dataFrame = Lorenz5D, lib = "1 500", pred = "601 900", columns = "V1 V2 V3 V4", target = "V1", embedded = TRUE, showPlot = TRUE)
PredictNonlinear(dataFrame = Lorenz5D, lib = "1 500", pred = "601 900", E = 4, columns = "V1", target = "V1", embedded = FALSE, showPlot = TRUE)
PredictNonlinear(dataFrame = Lorenz5D, lib = "1 500", pred = "601 900", E = 4, columns = "V1 V2 V3 V4", target = "V1", embedded = TRUE, showPlot = TRUE)

 # Why use E = 4, theta = 3?
smap_Lorenz<- SMap(dataFrame = Lorenz5D, lib = "1 500", pred = "601 900", E = 4,
                   theta = 3, columns = "V1 V2 V3 V4", target = "V1", embedded = TRUE)

head(cbind(smap_Lorenz$predictions, smap_Lorenz$coefficients[, 2:6]))

predictions = smap_Lorenz$predictions
coefficients = smap_Lorenz$coefficients
Time = predictions$Time
par(mfrow=c(2,2))
plot(Time, predictions$Observations, type = "l", col = "blue", ylab = "V1", xlab = "",
     lwd = 2, cex.lab = 1.3, cex.axis = 1.3)
lines(Time, predictions$Predictions, lwd = 2, col = "red")
legend("topright", legend = c("observed", "predicted"), fill = c("blue", "red"),
       bty = "n", cex = 1.3)
plot(Time, coefficients[, 6], type = "l", col = "brown", ylab = paste("", "V4/","", "V1", sep = ""), xlab = "", lwd = 2, cex.lab = 1.3, cex.axis = 1.3)
plot(Time, coefficients[, 5], type = "l", col = "darkgreen", ylab = paste("","V3/", "", "V1", sep = ""), xlab = "", lwd = 2, cex.lab = 1.3, cex.axis = 1.3)
plot(Time, coefficients[, 4], type = "l", col = "blue", ylab = paste("", "V2/","", "V1", sep = ""), xlab = "", lwd = 2, cex.lab = 1.3, cex.axis = 1.3)

#Multiview embedding
Mview = Multiview(dataFrame = block_3sp, lib = "1 100", pred = "101 190", E = 3,
                  columns = "x_t y_t z_t", target = "x_t")
Mview$View[which(Mview$View$rho > 0.91), ]

# rEDM vignette - sardine/anchovy CCM  & thrips example --------------------------------------------------------------
# http://127.0.0.1:22304/library/rEDM/doc/rEDM-tutorial.pdf

str(sardine_anchovy_sst)
EmbedDimension(dataFrame = sardine_anchovy_sst, lib = "1 72", pred = "1 72", columns = "anchovy", target = "anchovy", showPlot = TRUE) #my test

cmap <- CCM(dataFrame = sardine_anchovy_sst, E = 3, Tp = 0, columns = "anchovy",
            target = "np_sst", libSizes = "10 70 5", sample = 100, showPlot = TRUE)
names(cmap)

head(Thrips)
str(Thrips)

rho_E <- EmbedDimension(dataFrame = Thrips, lib = "1 72", pred = "1 72", columns = "Thrips_imaginis",target = "Thrips_imaginis")
rho_E[which.max(rho_E$rho),"E"] #3 or 8
rho_theta <- PredictNonlinear(dataFrame = Thrips, lib = "1 72", pred = "1 72", columns = "Thrips_imaginis",target = "Thrips_imaginis", E = 8)
rho_theta[which.max(rho_theta$rho), "Theta"] #optimal theta = 3
PredictInterval(dataFrame = Thrips, lib = "1 72", pred = "1 72", columns = "Thrips_imaginis",target = "Thrips_imaginis", E = 8)

E = 8
vars = colnames(Thrips[3:6])
var_pairs = combn(vars, 2) # Combinations of vars, 2 at a time
libSize = paste(NROW(Thrips) - E, NROW(Thrips) - E, 10, collapse = " ")
ccm_matrix = array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars,vars))

for (i in 1:ncol(var_pairs)) {
  ccm_out = CCM(dataFrame = Thrips, columns = var_pairs[1, i], target = var_pairs[2,i], #columns is top row in var_pairs matrix
                libSizes = libSize, Tp = 0, E = E, sample = 100)
  outVars = names(ccm_out)
  var_out = unlist(strsplit(outVars[2], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 2]
  var_out = unlist(strsplit(outVars[3], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 3]
}

#For comparison we also compute the lagged cross-correlation, allowing lags of up to ±6 months.
corr_matrix <- array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars,vars))
for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    ccf_out <- ccf(Thrips[, ccm_from], Thrips[, ccm_to], type = "correlation",
                   lag.max = 6, plot = FALSE)$acf
    corr_matrix[ccm_from, ccm_to] <- max(abs(ccf_out)) }
                              }

par(mfrow=c(2,2))
thrips_xmap_maxT <- CCM(dataFrame = Thrips, E = E, Tp = 0, columns = "Thrips_imaginis",
                        target = "maxT_degC", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Thrips_imaginis", "maxT_degC"], col = "black", lty = 2)
thrips_xmap_maxT <- CCM(dataFrame = Thrips, E = E, Tp = 0, columns = "Thrips_imaginis",
                        target = "Rain_mm", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Thrips_imaginis", "Rain_mm"], col = "black", lty = 2)
thrips_xmap_maxT <- CCM(dataFrame = Thrips, E = E, Tp = 0, columns = "Thrips_imaginis",
                        target = "Season", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Thrips_imaginis", "Season"], col = "black", lty = 2)

#Seasonal surrogate test
# Create matrix with temperature and rain surrogates (1000 time series vectors)
surr_maxT = SurrogateData(Thrips$maxT_degC, method = "seasonal", T_period = 12, num_surr = 1000,
                          alpha = 3)
surr_rain = SurrogateData(Thrips$Rain_mm, method = "seasonal", T_period = 12, num_surr = 1000,
                          alpha = 3)

# Rain cannot be negative
surr_rain = apply(surr_rain, 2, function(x) {
  i = which(x < 0)
  x[i] = 0
  x
})
# data.frame to hold CCM rho values between Thrips abundance and variable
rho_surr <- data.frame(maxT = numeric(1000), Rain = numeric(1000))

# data.frames with time, Thrips, and 1000 surrogate climate variables for CCM()
maxT_data = as.data.frame(cbind(seq(1:nrow(Thrips)), Thrips$Thrips_imaginis, surr_maxT))

names(maxT_data) = c("time", "Thrips_imaginis", paste("T", as.character(seq(1, 1000)),
                                                      sep = ""))

rain_data = as.data.frame(cbind(seq(1:nrow(Thrips)), Thrips$Thrips_imaginis, surr_rain))

names(rain_data) = c("time", "Thrips_imaginis", paste("R", as.character(seq(1, 1000)),
                                                      sep = ""))
#cross mapping
for (i in 1:1000) {
  targetCol = paste("T", i, sep = "") # as in maxT_data
  ccm_out = CCM(dataFrame = maxT_data, E = E, Tp = 0, columns = "Thrips_imaginis",
                target = targetCol, libSizes = "73 73 5", sample = 1)
  col = paste("Thrips_imaginis", ":", targetCol, sep = "")
  rho_surr$maxT[i] = ccm_out[1, col]
}

for (i in 1:1000) {
  targetCol = paste("R", i, sep = "") # as in rain_data
  ccm_out = CCM(dataFrame = rain_data, E = E, Tp = 0, columns = "Thrips_imaginis",
                target = targetCol, libSizes = "73 73 5", sample = 1)
  col = paste("Thrips_imaginis", ":", targetCol, sep = "")
  rho_surr$Rain[i] = ccm_out[1, col]
}

1 - ecdf(rho_surr$maxT)(ccm_matrix["maxT_degC", "Thrips_imaginis"])
1 - ecdf(rho_surr$Rain)(ccm_matrix["Rain_mm", "Thrips_imaginis"])


# Sunspots ----------------------------------------------------------------
#https://github.com/SugiharaLab/rEDM 
str(sunspots)

df = data.frame(yr = as.numeric(time(sunspot.year)), 
                sunspot_count = as.numeric(sunspot.year))
plot(df$yr, df$sunspot_count, type = "l", 
     xlab = "year", ylab = "sunspots")
E.opt = EmbedDimension( dataFrame = df,    # input data
                        lib     = "1 280", # portion of data to train
                        pred    = "1 280", # portion of data to predict
                        columns = "sunspot_count",
                        target  = "sunspot_count" )

simplex = Simplex( dataFrame = df, 
                   lib     = "1   190", # portion of data to train
                   pred    = "191 287", # portion of data to predict
                   columns = "sunspot_count",
                   target  = "sunspot_count",
                   E       = 3 )
plot( df$yr, df$sunspot_count, type = "l", lwd = 2,
      xlab = "year", ylab = "sunspots")
lines( simplex$yr, simplex$Predictions, col = "red", lwd = 2)
legend( 'topleft', legend = c( "Observed", "Predicted (year + 1)" ),
        fill = c( 'black', 'red' ), bty = 'n', cex = 1.3 )
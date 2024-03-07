# Reproducible example of question about ComputeError function
# Ruby Krasnow, 2024-03-07

library(rEDM)
library(Metrics)


# Define vectors with NaN in the same place as they are in SMap output

obs <- c(
11.2900207891833,
11.9955387159498,
2.90124689864347,
5.69309777063509,
1.98526052623628,
4.36820622210878,
1.59129400298131,
2.49683091246559,
2.03230147417917,
1.00295475815157,
1.50837232694949,
0.771408307064201,
1.50734557261394,
1.38601236017698,
4.25503177106659,
3.07370141038872, NaN)

preds <- c(NaN,
           18.7064937666589,
           3.71757535082661,
           5.62886649768187,
           2.26995667789194,
           6.61816035554659,
           5.74245007762128,
           2.32047406652773,
           7.48665890589844,
           -0.142253761273642,
           1.49136876608757,
           2.33088308425496,
           2.57013804225349,
           6.77439555793835,
           5.57396696782763,
           7.50327440347941,
           2.11173474007841)

# subsets without NaN
obs[1:16]
preds[2:17]

# Why are these different?
ComputeError(obs, preds)
ComputeError(obs[1:16], preds[2:17]) #this one is much worse

# These agree with the second, less optimistic ComputeError results
cor(obs[1:16], preds[2:17])
rmse(obs[1:16], preds[2:17])
mae(obs[1:16], preds[2:17])

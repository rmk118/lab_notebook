library(tidyverse)
library(rEDM)
data( block_3sp )

df_1 <- bind_rows(block_3sp[1:100,],block_3sp[1:100,]) %>% mutate(ID=c(rep(1,100),rep(2,100)))
df_2 <- bind_rows(block_3sp[1:100,],block_3sp[1:100,]) %>% mutate(ID=c(rep(1,100),rep(2,100)))
df_2[5,"x_t"] = 0

df_1[5,]
df_2[5,]

v_lib <- rep(TRUE,200)
# v_lib[4] <- FALSE
v_lib[5] <- FALSE


do_univ_once <- function(data,target,ID_col,lib,pred_indices,E,E_max=8,Tp=1,exclude=0){
  
  # identify which library indices will correspond to concatenation breaks
  v_new_chunks <- ( lag(data[[ID_col]]) == data[[ID_col]] )
  v_valid_lib_0 <- #( lead(v_new_chunks,Tp) &
                    (reduce(map(0:(E_max-1),~lag(lead(v_new_chunks),.)),`&`) )
  #return(v_new_chunks)
 # return(v_valid_lib_0)
  
  # Simplex
  out_simplex <- map_dfr(pred_indices,function(pred_i){

    v_valid_lib_i <- v_valid_lib_0

    if(is.finite(exclude)){
      I_overlap_times <- abs(data[,1] - data[pred_i,1]) <= exclude + 10^-8
      v_valid_lib_i[I_overlap_times] <- F
    }

    Simplex(dataFrame=data,lib=lib,pred=paste(pred_i-1,pred_i),
            columns = target,target=target,E=E,
            validLib = v_valid_lib_i) %>% tail(n=1)

  })

  return(out_simplex)
  # SMap
  
}

if(FALSE){
  ## Do a laborious but careful check

  out_1_standard <- Simplex(dataFrame=df_1,lib="1 100",pred="102 110",E=3,Tp=0,columns = "x_t-1",target = "x_t")
  out_2_standard <- Simplex(dataFrame=df_2,lib="1 100",pred="102 110",E=3,Tp=0,columns = "x_t-1",target = "x_t")
  
  data.frame(time=out_1_standard$time[4:6],
             df_1=out_1_standard$Predictions[4:6],
             df_2=out_2_standard$Predictions[4:6])
  
  
  out_1_standard_vlib <- Simplex(dataFrame=df_1,lib="1 100",pred="102 110",E=3,Tp=0,columns = "x_t-1",target = "x_t",validLib = v_lib)
  out_2_standard_vlib <- Simplex(dataFrame=df_2,lib="1 100",pred="102 110",E=3,Tp=0,columns = "x_t-1",target = "x_t",validLib = v_lib)
  
  data.frame(time=out_1_standard_vlib$time[4:6],
             df_1=out_1_standard_vlib$Predictions[4:6],
             df_2=out_2_standard_vlib$Predictions[4:6])
  
  out_1_modified <- do_univ_once(data = df_1,ID_col = "ID",target="x_t",lib = "1 100",pred_indices = 101:110,E=3,E_max = 5,exclude = 0)
  out_2_modified <- do_univ_once(data = df_2,ID_col = "ID",target="x_t",lib = "1 100",pred_indices = 101:110,E=3,E_max = 5,exclude = 0)
  out_3_modified <- do_univ_once(data = df_2,ID_col = "ID",target="x_t",lib = "1 100",pred_indices = 101:110,E=3,E_max = 5,exclude = NA) # should give same result as standard for row index 4
  
  data.frame(time=out_1_modified$time[4:6],
             df_1=out_1_modified$Predictions[4:6],
             df_2=out_2_modified$Predictions[4:6],
             df_3=out_3_modified$Predictions[4:6])
}

out_4_modified <- do_univ_once(data = df_1,ID_col = "ID",target="x_t",lib = "1 100",pred_indices = 101:110,E=3,E_max = 5,exclude = NA)

data.frame(time=out_1_modified$time[4:6],
           df_1=out_1_modified$Predictions[4:6],
           df_2=out_2_modified$Predictions[4:6],
           df_3=out_3_modified$Predictions[4:6],
           df_4 = out_4_modified$Predictions[4:6])



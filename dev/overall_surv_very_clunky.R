library(devtools)

load_all()

# test_mu <- 

mu_test <- sapply(c(2,2,2,-1,0.5,0.1),function(x) rnorm(50,mean = x,sd=0.1))  
sigma_test <- sapply(c(0.2,0.05,0.1,0.05,0.05,1),function(x) rnorm(50,mean = x,sd=0))

get_overall_surv(MU_mat = matrix(c(2,2,2,-1,0.5,0.1),nrow=1),
                 SIGMA2_mat = matrix(c(0.2,0.05,0.1,0.05,0.05,1),nrow=1)
                 )

get_overall_surv_sim(MU_mat = matrix(c(2,2,2,-1,0.5,0.1),nrow=1),
                 SIGMA2_mat = matrix(c(0.2,0.05,0.1,0.05,0.05,1),nrow=1),n_samples = 50)

out <- get_overall_surv(MU_mat = mu_test,
                 SIGMA2_mat = sigma_test)


out_sim <- get_overall_surv_sim(MU_mat = mu_test,
                        SIGMA2_mat = sigma_test,n_samples = 50)

library(dplyr)
library(tidyR)

# matplot(data.frame(id=1:50,type="sim",out_sim[,grep(names(out_sim),pattern="_mean|_LCL|_UCL")])[,-1],type="b",pch=21)

# comb_out<- rbind(
#   reshape2::melt(data.frame(id=1:50,type="sim",out_sim[,grep(names(out_sim),pattern="_mean|_LCL|_UCL")]),1:2),
#   reshape2::melt(data.frame(id=1:50,type="analytical",out_sim[,grep(names(out_sim),pattern="_mean|_LCL|_UCL")]),1:2))

comb_out
ggplot(data=comb_out,aes(x=id,y=value,color=value)) + geom_point()


# data.frame(type="sim",out_sim[,grep(names(out_sim),pattern="_mean|_LCL|_UCL")]) |> tidyr::pivot_longer(names_to="type")


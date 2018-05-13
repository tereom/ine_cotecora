library(quickcountmx)
library(rstan)
library(tidyverse)
library(shinystan)
data("nal_2012")


sm <- stan_model(file = "./stan_models/neg_binomial_fast.stan",
                 save_dso = TRUE)
partidos <- c("pri_pvem", "pan", "panal", "prd_pt_mc", "otros")

nal_sample <- select_sample_prop(nal_2012, 
                                 stratum = estrato, 0.052) 

seed <- 3445
clust <-  parallel::makeCluster(getOption("cl.cores", 5))
parallel::clusterSetRNGStream(clust, seed)
parallel::clusterExport(clust, c("nal_2012", "nal_sample", "sm"))
parallel::clusterEvalQ(clust, {
  library(dplyr)
  library(quickcountmx)
  library(rstan)
})

models_party <- parallel::parLapply(clust, 1:5, function(i){
  party_name <- partidos[i]
  print(party_name)

  data_model <- nal_sample
  x <- as.matrix(data_model %>% ungroup %>% select(rural, tamano_md, tamano_gd))
  data_sample <- list(N = nrow(data_model), n = data_model$ln_total,
                        n_covariates = ncol(x),
                        n_strata = length(unique(data_model$estrato)),
                        y = data_model[, party_name][[1]],
                        stratum = dplyr::pull(data_model, estrato),
                        x = x
    )
    data <- nal_2012
    x_full <- as.matrix(data %>% ungroup %>% select(rural, tamano_md, tamano_gd))
    data_full <- list(N_f = nrow(data), n_f = data$ln_total,
                      n_covariates_f = ncol(x),
                      in_sample = as.numeric(data$casilla_id %in% data_model$casilla_id),
                      n_strata_f = length(unique(data$estrato)),
                      y_f = data[, party_name][[1]],
                      stratum_f = dplyr::pull(data, estrato),
                      x_f = x_full)
    
    fit <- sampling(sm, iter = 300, warmup = 150, thin = 1,
                    chains = 1, data= c(data_sample, data_full), cores = 1)
    rstan::extract(fit, 'y_out')[[1]]
})

parallel::stopCluster(clust)
###
df_out <- as.data.frame(models_party)
names(df_out) <- partidos
df_out$sim_no <- 1:nrow(df_out)
df_out$rep <- 1
sims <- as_tibble(df_out)

sims_res <- sims %>% gather(party, votes, -sim_no, -rep) %>%
  group_by(rep, sim_no) %>%
  mutate(prop = votes / sum(votes)) %>%
  group_by(rep, party) %>%
  summarise(mean_post = 100*mean(prop), std_post = 100*sd(prop))

actual <- nal_2012 %>% select(pri_pvem:otros, casilla_id) %>%
  gather(party, value, -casilla_id) %>%
  group_by(party) %>%
  summarise(total = sum(value)) %>%
  mutate(p_total = 100*total/sum(total)) 

sims_res <- sims_res %>% left_join(actual)
ggplot(sims_res, aes(x=rep, y = mean_post, ymin = mean_post - 2*std_post,
                     ymax = mean_post + 2*std_post)) +
  geom_linerange() + facet_wrap(~party, scales = 'free_y') +
  geom_hline(aes(yintercept = p_total), colour = "red")

sims_res %>% mutate(coverage = p_total > mean_post - 2*std_post &
                      p_total < mean_post + 2*std_post) %>%
  group_by(party) %>% summarise(coverage = mean(coverage),
                                precision = 2*mean(std_post))



## Co
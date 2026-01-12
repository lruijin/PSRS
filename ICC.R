setwd("/Projects/Brad/PSRS")
.libPaths("/Projects/r.lu")
psrs <- read.csv("PSRS_clean.csv")
library(dplyr)
library(brms)
library(reshape2)

# restructure data
names(psrs)[1:3] <- c("rater","trial", "domain")
df <- melt(psrs, id.vars = c(c("rater","trial", "domain"))) %>%
  rename(subject = variable,
         score = value) %>%
  mutate(subject = sprintf("%02d",as.integer(gsub("X","",subject))))
domains = unique(df$domain)
# Fit Bayesian ordinal mixed model
inter_res <- intra_res <- NULL
for(i in seq_along(domains)[-length(domains)]){
  df_domain = df %>% filter(domain == domains[i])
  df_domain <- df_domain %>%
    mutate(score = factor(as.integer(score), ordered = T))
  fit <- brm(
    score ~ 1 + (1 | subject) + (1 | rater) + (1 | subject:rater),
    data = df_domain,
    family = cumulative(link = "logit"),
    chains = 2,
    iter = 4000,
    cores = 2,
    seed = 518
  )
  save(fit, file = paste0("fitted_models/model_", domains[i], ".Rdata"))
  # Check diagnostics
  # summary(fit)
  # plot(fit)
  # pp_check(fit)
  
  # Extract posterior draws
  posterior <- as_draws_df(fit)
  
  # Variance components
  var_subject <- posterior$sd_subject__Intercept^2
  var_rater <- posterior$sd_rater__Intercept^2
  var_interaction <- posterior$`sd_subject:rater__Intercept`^2
  var_logistic <- pi^2 / 3  # Logistic distribution residual variance
  
  # Compute ordinal reliability (Bayesian G-coefficient)
  inter_rater <- var_subject / 
    (var_subject + var_rater + var_interaction + var_logistic)
  intra_rater <- (var_subject + var_rater + var_interaction)/
    (var_subject + var_rater + var_interaction + var_logistic)
  
  # Summarize reliability
  inter_res <- rbind(inter_res,
                     c(mean(inter_rater),
                       quantile(inter_rater, c(0.025, 0.975))))
  intra_res <- rbind(intra_res,
                     c(mean(intra_rater),
                       quantile(intra_rater, c(0.025, 0.975))))
}


# Optional: Inspect cutpoints
cat("Estimated ordinal cutpoints:\\n")
print(posterior_summary(fit, pars = "^b_Intercept"))

### ------------- Total score -----------------
fit <- brm(
  score ~ 1 + (1 | subject) + (1 | rater) + (1 | subject:rater),
  data = df %>% filter(domain == "SUM"),
  family = gaussian(),  
  chains = 2,
  iter = 4000,
  cores = 2,
  seed = 518
)
save(fit, file = paste0("fitted_models/model_sum.Rdata"))
# Check diagnostics
# summary(fit)
# plot(fit)
# pp_check(fit)

# Extract posterior draws
posterior <- as_draws_df(fit)

# Variance components
var_subject <- posterior$sd_subject__Intercept^2
var_rater <- posterior$sd_rater__Intercept^2
var_interaction <- posterior$`sd_subject:rater__Intercept`^2
var_logistic <- pi^2 / 3  # Logistic distribution residual variance

# Compute ordinal reliability (Bayesian G-coefficient)
inter_rater <- var_subject / 
  (var_subject + var_rater + var_interaction + var_logistic)
intra_rater <- (var_subject + var_rater + var_interaction)/
  (var_subject + var_rater + var_interaction + var_logistic)

# Summarize reliability
inter_res <- rbind(inter_res,
                   c(mean(inter_rater),
                     quantile(inter_rater, c(0.025, 0.975))))
intra_res <- rbind(intra_res,
                   c(mean(intra_rater),
                     quantile(intra_rater, c(0.025, 0.975))))
write.csv(data.frame(domain = domains, as.data.frame(inter_res)),
          file = "inter-rater.csv", row.names = F)
write.csv(data.frame(domain = domains, as.data.frame(intra_res)),
          file = "intra-rater.csv", row.names = F)



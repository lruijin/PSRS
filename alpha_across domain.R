setwd("C:/Users/r.lu/Box/Brad/PSRS")
# .libPaths("/Projects/r.lu")
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
table(df$rater)
df_mean <- df %>% group_by(domain, subject) %>%
  summarise(score = mean(score), .groups = "keep")
library(psych)
domains = names(table(df_mean$domain))
domains <- setdiff(domains, "SUM")

df_mean_wide <- data.frame(subject = unique(df_mean$subject)) 
for(i in seq_along(domains)){
  d = domains[i]
  df_mean_wide <- df_mean_wide %>% 
    left_join(df_mean %>% filter(domain == d) %>% ungroup() %>%
                select(subject, score), by = "subject")
  names(df_mean_wide)[i + 1] <- d
}
df_mean_wide

a = alpha(df_mean_wide[,-1], discrete = F)
a$total$raw_alpha
a.ci <- alpha.ci(a$total$raw_alpha, n.obs = 20, n.var = length(domains))

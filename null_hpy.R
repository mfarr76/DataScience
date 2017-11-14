install.packages("NHANES")
install.packages("dplyr")
rm(list = ls())

library(dplyr)
library(ggplot2)
library(NHANES)

rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1)
{
  n <- nrow(tbl)
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace), simplify = FALSE))
  rep_tbl <- cbind(replicate = rep(1:reps,rep(size,reps)), tbl[i,])
  dplyr::group_by(rep_tbl, replicate)
}



pricedata <- matrix(c(102, 90, 84, 130, 45), ncol=1)
colnames(pricedata) <- c('Price')
rownames(pricedata) <- c('2012', '2013', '2014', '2015', '2016')
pricedata.table <- as.table(pricedata)
pricedata.table


homes <- NHANES %>%
  select(Gender, HomeOwn) %>%
  filter(HomeOwn %in% c("Own", "Rent"))

unique(NHANES$HomeOwn)

ggplot(NHANES, aes(x = Gender, fill = HomeOwn)) + 
  geom_bar(position = "fill") +
  ylab("Relative frequencies")

homes %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own))

HomeOwn_perm = sample(NHANES$HomeOwn)
(x1 <- 1:20)
sample(x1)
diff(x1)

homeown_perm <- homes %>%
  rep_sample_n(size = nrow(homes), reps = 100) %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(replicate, Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own)) # male - female

# Dotplot of 100 permuted differences in proportions
ggplot(homeown_perm, aes(x = diff_perm)) + 
  geom_dotplot(binwidth = .001)


# Perform 1000 permutations
homeown_perm <- homes %>%
  rep_sample_n(size = nrow(homes), reps = 1000) %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(replicate, Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own)) # male - female

# Density plot of 1000 permuted differences in proportions
ggplot(homeown_perm, aes(x = diff_perm)) + 
  geom_density()


disc <- data.frame(
  promote = c(rep("promoted", 35), rep("not_promoted", 13)),
  sex = c(rep("male", 21), rep("female", 14),
          rep("male", 3), rep("female", 10)))
disc %>%
  group_by(sex) %>%
  summarise(promoted_prop = mean(promote == "promoted"))

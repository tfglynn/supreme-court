library(dplyr)
library(forcats)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(magrittr)
library(readr)
library(rstan)
library(tidyr)

rstan_options(auto_write = TRUE)

##################################################
# Loading the data
##################################################

# TODO: We don't use all of these, but they might be useful later.
columns <-
  c("term", "caseName", "caseId",
    "petitioner", "petitionerState",
    "respondent", "respondentState",
    "jurisdiction",
    "adminAction", "adminActionState",
    "caseOrigin", "caseOriginState",
    "caseSource", "caseSourceState",
    "certReason",
    "lcDisagreement",
    "lcDisposition",
    "lcDispositionDirection",
    "issue", "issueArea",
    "decisionType",
    "caseDisposition", "caseDispositionUnusual",
    "justiceName", "justice",
    "dateDecision",
    "vote", "voteUnclear")

raw_data <-
  read_csv("SCDB_2021_01_justiceCentered_Citation.csv",
           col_select = all_of(columns))

##################################################
# Preparing the dataframe
##################################################

# The MQ paper says they excluded cases coded with decisionType 2, per curiam.
# I'm not sure why.  These cases seem to have plenty of useful information.

outcomes <-
  c("granted", "affirmed", "reversed", "reversed+remanded",
    "vacated+remanded", "mixture", "mixture+remanded", "vacated",
    "dismissed", "certification", "none")
affirmations <- c("affirmed", "dismissed")
reversions <- c("reversed", "reversed+remanded", "vacated", "vacated+remanded")

stan_data <-
  raw_data %>%
  drop_na(caseDisposition, lcDispositionDirection) %>%
  mutate(caseDisposition = outcomes[caseDisposition],
         dateDecision = mdy(dateDecision)) %>%
  select(caseName, caseId, term, # Identifying information
         justice, justiceName,   # Justice identifiers
         lcDispositionDirection, # Lower court decision conservative or liberal?
         caseDisposition,        # How SCOTUS decided
         vote, voteUnclear) %>%  # How each justice voted

  # Remove rows with unclear votes, unclear political leanings,
  # or an unclear outcome.
  filter(caseDisposition %in% c(affirmations, reversions),
         voteUnclear != 1,
         (vote %in% 1:7) & !is.na(vote),
         lcDispositionDirection != 3) %>%
  select(-voteUnclear) %>%

  # Figure out who voted to affirm the lower court's decision.
  mutate(reversed = if_else(caseDisposition %in% affirmations, 0, 1),
         lcDispositionDirection = if_else(lcDispositionDirection == 2, -1, 1),
         vote = if_else(vote %in% c(1, 3, 4, 5), 1, 0),
         voteAffirm = as.integer(xor(vote, reversed))) %>%

  # Some cases have no votes recorded, so we filter those out.
  #group_by(caseId) %>%
  #mutate(percNA = sum(is.na(vote)) / length(vote)) %>%
  #filter(percNA < 1) %>%
  #ungroup %>%

  # Work with a fraction of the data.  This is mostly to save time.
  filter(term >= 2000) %>%

  # We have to keep the justices sorted within each case for the Stan program
  # to work.
  mutate(id = match(justice, unique(justice))) %>%
  group_by(caseId) %>%
  arrange(id, .by_group = TRUE) %>%
  ungroup

##################################################
# Setting up Stan variables
##################################################

num_terms    <- n_distinct(stan_data$term)
num_cases    <- n_distinct(stan_data$caseId)
num_justices <- n_distinct(stan_data$id)
num_votes    <- nrow(stan_data)
num_thetas   <- sum(
  stan_data %>%
  select(id, term) %>%
  group_by(term) %>%
  distinct %>%
  count %>%
  `$`("n")
)

cases_in_term <-
  stan_data %>%
  select(caseId, term) %>%
  group_by(term) %>%
  distinct %>% # we have duplicates because there's one row per vote
  count %>%
  `$`("n")
justices_in_term <-
  stan_data %>%
  select(id, term) %>%
  group_by(term) %>%
  distinct %>%
  count %>%
  `$`("n")
votes_in_case <-
  stan_data %>%
  group_by(caseId) %>%
  count %>%
  `$`("n")

theta_df <-
  stan_data %>%
  select(id, term) %>%
  distinct %>%
  count(id)
theta_pos <- theta_df %>% mutate(init = cumsum(n) - n + 1) %>% `$`("init")
theta_num <- theta_df$n
theta_name <- # to help interpreting the model later
  stan_data %>%
  select(id, justiceName) %>%
  distinct %>%
  `$`("justiceName") %>%
  rep(theta_num)

justice <-
  stan_data %>%
  select(id, term) %>%
  group_by(term) %>%
  arrange(id, .by_group = TRUE) %>%
  distinct %>%
  `$`("id")
vote  <- stan_data$voteAffirm
voter <- stan_data$id
lccon <-
  stan_data %>%
  select(caseId, lcDispositionDirection) %>%
  distinct %>%
  `$`("lcDispositionDirection")

##################################################
# Running the model
##################################################

# This takes several minutes on one core
# if using all data from years >= 2000.

fit <-
  stan("sc.stan",
       cores = 2,
       data = list(# Sizes of arrays
                   TERM_COUNT = num_terms,
                   CASE_COUNT = num_cases,
                   JUSTICE_COUNT = num_justices,
                   VOTE_COUNT = num_votes,
                   THETA_COUNT = num_thetas,

                   # Counts of cases/justices/votes
                   cases_in_term = cases_in_term,
                   justices_in_term = justices_in_term,
                   votes_in_case = votes_in_case,

                   # The beginning index and size of each justice's
                   # block of parameters in the array
                   theta_pos = theta_pos,
                   theta_num = theta_num,

                   justice = justice, # IDs of justices in each term
                   vote = vote,       # Affirm (1) or reverse (0)
                   voter = voter,     # ID array to parallel the vote array
                   lccon = lccon))    # Lower court lib (-1) or con (1)

##################################################
# Plotting results
##################################################

samples <- extract(fit, pars = "theta") %>% `$`("theta")

level <- 0.50
means <- colMeans(samples)
hi <- apply(samples, 2, . %>% quantile(1 - (1 - level)/2))
lo <- apply(samples, 2, . %>% quantile((1 - level)/2))

tib <-
  tibble(name = theta_name,
         mean = means,
         hi = hi, lo = lo) %>%
  arrange(name)
tib$term <-
  stan_data %>%
  select(term, justiceName) %>%
  distinct %>%
  arrange(justiceName) %>%
  `$`("term")

tib %>%
  filter(name != "ACBarrett") %>% # not enough data
  group_by(name) %>%
  mutate(label = if_else(term == max(term), name, NA_character_)) %>%
  ungroup %>%
  ggplot(aes(term, mean, color = name)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = name), alpha = 0.1, color = NA) +
  geom_point() +
  geom_line() +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
  theme(legend.position = "none") +
  labs(x = "Term", y = "Coefficient",
       title = "Coefficient of conservatism over time",
       subtitle = "Ribbons are 50% credible intervals")
ggsave("graph.png")

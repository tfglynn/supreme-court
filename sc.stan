data {
  // Sizes
  int<lower=0> TERM_COUNT;    // number of terms
  int<lower=0> CASE_COUNT;    // number of cases
  int<lower=0> JUSTICE_COUNT; // number of justices
  int<lower=0> VOTE_COUNT;    // number of votes
  int<lower=0> THETA_COUNT;   // number of (justice, term) pairs

  // Counts, not the actual data
  int<lower=0> cases_in_term[TERM_COUNT];    // number of cases for each term
  int<lower=0> justices_in_term[TERM_COUNT]; // number of justices for each term
  int<lower=0> votes_in_case[CASE_COUNT];    // number of vote(r)s in each case

  // Used to organize the parameters
  int<lower=0> theta_pos[JUSTICE_COUNT]; // index of first theta associated with each justice
  int<lower=0> theta_num[JUSTICE_COUNT]; // number of terms each justice served (also # of thetas they get)

  // The actual data
  int<lower=0> justice[THETA_COUNT]; // flat array of justice IDs for each term
  int vote[VOTE_COUNT];              // 0 = reverse, 1 = affirm
  int<lower=0> voter[VOTE_COUNT];    // justice ID for each vote
  vector[CASE_COUNT] lccon;          // lower court's decision was conservative?
}

parameters {
  // logit(p) = alpha + theta * lccon, where p is probability of affirming
  vector[CASE_COUNT] alpha;  // per-case intercept, ~ how likely a neutral judge is to affirm
  vector[THETA_COUNT] theta; // how much each justice prefers a conservative outcome
}

model {
  int current_theta_index[JUSTICE_COUNT];
  int justice_pos; // index into "justice" array
  int case_pos;    // index into alpha, etc.
  int vote_pos;    // index into "vote" array

  for (j in 1:JUSTICE_COUNT) {
    int pos;
    pos = theta_pos[j];
    current_theta_index[j] = pos;
    theta[pos] ~ normal(0, 3); // TODO: add demographic variables here?
  }

  justice_pos = 1;
  case_pos = 1;
  vote_pos = 1;

  for (t in 1:TERM_COUNT) {
    int justice_count;
    int case_count;
    int vote_count;

    justice_count = justices_in_term[t];
    case_count = cases_in_term[t];
    vote_count = 0;
    for (k in case_pos:(case_pos + case_count - 1)) {
      vote_count = vote_count + votes_in_case[k];
    }

    { // Start a new block so we can make variable declarations here.
      vector[case_count] case_alpha;
      vector[case_count] case_lccon;
      vector[vote_count] log_odds_vector;
      matrix[case_count, justice_count] log_odds_matrix;
      int term_voter[vote_count];
      int term_justice[justice_count];
      vector[justice_count] current_theta;
      matrix[case_count, justice_count] errors;
      int ix; // used in a nested loop below

      term_justice = segment(justice, justice_pos, justice_count);
      current_theta = theta[current_theta_index[term_justice]];

      segment(alpha, case_pos, case_count) ~ normal(0, 3); // TODO: what's a good scale?  add features of case here?
      case_alpha = segment(alpha, case_pos, case_count);
      term_voter = segment(voter, vote_pos, vote_count);
      case_lccon = segment(lccon, case_pos, case_count);

      // Each row is a case, and each column is a justice.
      // This calculation assumes every justice votes on every case,
      // which is untrue.  So we need to pick out the actual voters
      // after the fact.
      log_odds_matrix = case_lccon * current_theta';
      for (j in 1:justice_count) {
        log_odds_matrix[, j] = log_odds_matrix[, j] + case_alpha;
      }
      ix = 1;
      for (k in 1:case_count) {
        for (j in 1:justice_count) {
          if (term_voter[ix] == term_justice[j]) {
            log_odds_vector[ix] = log_odds_matrix[k, j];
            if (ix == vote_count) {
              break;
            }
            ix = ix + 1;
          }
        }
      }
      segment(vote, vote_pos, vote_count) ~ bernoulli_logit(log_odds_vector);

      // Update the thetas of justices who are going to continue.
      for (j in term_justice) {
        int pos;
        pos = current_theta_index[j];
        if (pos < theta_pos[j] + theta_num[j] - 1) {
          theta[pos + 1] ~ normal(theta[pos], 0.25); // TODO: do we want the random walk?  what scale?
          current_theta_index[j] = current_theta_index[j] + 1;
        }
      }

      justice_pos = justice_pos + justice_count;
      case_pos = case_pos + case_count;
      vote_pos = vote_pos + vote_count;
    }
  }
}

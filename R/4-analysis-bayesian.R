sn_sample_conf_in_tested <- function(n_sample, n_conf, n_test) {
  p_conf = n_conf / (n_conf + n_test)
  sample(c("T", "F"),
         size = n_sample,
         replace = TRUE,
         c(p_conf, 1 - p_conf))
}

sn_posterior_conf_in_tested <- function(sample, n) {
  sum_conf <- sum(sample == "T")
  sum_negative <- sum(sample == "F")
  p <- 0:n / n
  ways <- sapply(p, function(x) (x)^sum_conf * ((1-x))^sum_negative)
  sum_ways <- sum(ways)
  posterior <- ways / sum_ways
  result <- cbind(p, posterior)
  return(result)
}

sn_update_posterior <- function(posterior, sample) {

}

sample_1 <- sn_sample_conf_in_tested(1, 20, 50)
p_1 <- sn_posterior_conf_in_tested(sample_1, 100)
p_1 |> plot()
sample_2 <- sn_sample_conf_in_tested(2, 20, 50)
p_2 <- sn_posterior_conf_in_tested(sample_2, 100)
p_2 |> plot()
sample_3 <- sn_sample_conf_in_tested(3, 20, 50)
p_3 <- sn_posterior_conf_in_tested(sample_3, 100)
p_3 |> plot()
sample_4 <- sn_sample_conf_in_tested(4, 20, 50)
p_4 <- sn_posterior_conf_in_tested(sample_4, 100)
p_4 |> plot()

sample_5 <- sn_sample_conf_in_tested(5, 20, 50)
p_5 <- sn_posterior_conf_in_tested(sample_5, 100)
p_5 |> plot()
sample_6 <- sn_sample_conf_in_tested(6, 20, 50)
p_6 <- sn_posterior_conf_in_tested(sample_6, 100)
p_6 |> plot()

sample_20 <- sn_sample_conf_in_tested(20, 20, 50)
p_20 <- sn_posterior_conf_in_tested(sample_20, 100)
p_20 |> plot()

sample_100 <- sn_sample_conf_in_tested(100, 20, 50)
p_100 <- sn_posterior_conf_in_tested(sample_100, 100)
p_100 |> plot()

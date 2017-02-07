source("sims-code/sbm_funs3.R")

# For the  experiments, do you want to shove the dec vec up to one?
shove_dec <- TRUE

# Global settings
par_divs <- 19
par_seq_dec <- 1:par_divs / (par_divs + 1)
par_seq  <- round(100 * (1:par_divs / (par_divs + 1)))
par_dirs <- as.character(par_seq)


# Experiment 1 ------------------------------------------------------------

main_text <- "Increase N"
par_list <- make_param_list2()
pars <- c("N", "k", "max_k", "max_c", "min_c")
axis_par <- 1
par_settings <- matrix(0, 5, par_divs)
par_settings[1, ] <- round(2000 * (par_seq_dec + min(par_seq_dec) * 
                                     as.numeric(shove_dec)))
par_settings[2, ] <- sqrt(par_settings[1, ])
par_settings[3, ] <- 3 * par_settings[2, ]
par_settings[4, ] <- par_settings[1, ] * 3 / 10
par_settings[5, ] <- par_settings[4, ] * 2 / 3

save(par_list,
     main_text,
     axis_par,
     pars,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment1.RData")

# Experiment 2 ------------------------------------------------------------

main_text <- "Overall community sizes"
par_list <- make_param_list2()
pars <- c("c_scaler")
axis_par_string <- expression(c[1])
axis_par <- 1
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- par_seq_dec + min(par_seq_dec) * as.numeric(shove_dec)

save(par_list, 
     main_text,
     axis_par,
     pars,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     axis_par_string,
     file = "sims-results/sbm-par-lists/experiment2.RData")

# Experiment 3 ------------------------------------------------------------

main_text <- "Shrink degs"
par_list <- make_param_list2()
pars <- c("deg_scaler")
axis_par <- 1
axis_par_string <- expression(d[1])
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- par_seq_dec + min(par_seq_dec) * as.numeric(shove_dec)

save(par_list, 
     main_text,
     axis_par,
     pars,
     par_settings,
     axis_par_string,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment3.RData")

# Experiment 4 ------------------------------------------------------------

main_text <- "Increase edge signal"
par_list <- make_param_list2()
pars <- c("s2n_e")
axis_par_string <- expression(s[e])
axis_par <- 1
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- (min(par_seq_dec) * as.numeric(shove_dec) + 
                        par_seq_dec) * 2 + 1

save(par_list, 
     main_text,
     axis_par,
     pars,
     axis_par_string,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment4.RData")

# Experiment 5 ------------------------------------------------------------

main_text <- "Increase weight signal"
par_list <- make_param_list2()
pars <- c("s2n_w")
axis_par_string <- expression(s[w])
axis_par <- 1
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- (min(par_seq_dec) * as.numeric(shove_dec) + 
                        par_seq_dec) * 2 + 1

save(par_list, 
     main_text,
     axis_par,
     pars,
     par_settings,
     axis_par_string,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment5.RData")

# Experiment 6 ------------------------------------------------------------

main_text <- "Increase edge and weight signal"
par_list <- make_param_list2()
axis_par_string <- expression(s[e] ~ "and" ~ s[w])
pars <- c("s2n_e", "s2n_w")
axis_par <- 1
par_settings <- matrix(0, 2, par_divs)
par_settings[1, ] <- (min(par_seq_dec) * as.numeric(shove_dec) + 
                        par_seq_dec) * 2 + 1
par_settings[2, ] <- (min(par_seq_dec) * as.numeric(shove_dec) + 
                        par_seq_dec) * 2 + 1


save(par_list,
     main_text,
     axis_par,
     pars,
     axis_par_string,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment6.RData")


# Experiment 7 ------------------------------------------------------------

main_text <- "Increase # on"
par_list <- make_param_list2(om = 2)
pars <- c("on")
axis_par_string <- "# Overlap nodes"
axis_par <- 1
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- round((1 - (min(par_seq_dec) * as.numeric(shove_dec) + 
                                   par_seq_dec)) * 4000)

save(par_list,
     main_text,
     axis_par,
     pars,
     par_settings,
     axis_par_string,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment7.RData")

# Experiment 8 ------------------------------------------------------------

main_text <- "Increase om"
par_list <- make_param_list2(on = 500)
pars <- c("om")
axis_par_string <- "# Memberships per overlap node"
axis_par <- 1
par_settings <- matrix(0, 1, 4)
par_settings[1, ] <- 4:1
par_seq <- 1:4
par_divs <- 4
par_dirs <- as.character(par_seq)

save(par_list, 
     main_text,
     axis_par,
     pars,
     par_settings,
     axis_par_string,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment8.RData")

# Experiment 9 ------------------------------------------------------------

par_divs <- 19
par_seq <- seq(5, 95, 5)
par_dirs <- as.character(par_seq)
main_text <- "grow N, |BG|=1000, on=0.25N"
par_list <- make_param_list2(hv = 1000)
pars <- c("N", "k", "max_k", "max_c", "min_c", "on")
axis_par <- 1
par_settings <- matrix(0, 6, par_divs)
par_settings[1, ] <- round(5000 * (par_seq_dec + min(par_seq_dec) * 
                                     as.numeric(shove_dec)))
par_settings[2, ] <- sqrt(par_settings[1, ] + 1000)
par_settings[3, ] <- 3 * par_settings[2, ]
par_settings[4, ] <- par_settings[1, ] * 3 / 10
par_settings[5, ] <- par_settings[4, ] * 2 / 3
par_settings[6, ] <- 0.25 * par_settings[1, ]

save(par_list,
     main_text,
     axis_par,
     pars,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment9.RData")
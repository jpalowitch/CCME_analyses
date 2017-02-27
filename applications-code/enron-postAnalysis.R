saveDir <- "applications-results/enron/results"
dataDir <- "applications-results/enron/data"
library(Matrix)
library(igraph)
library(gdata)
library(xtable)
library(RColorBrewer)
source("osRead.R")
source("slpaRead.R")
source(file.path("applications-code", "get_statistics.R"))
# Brewing colors
colPal <- brewer.pal(9, "Set1")
colPal <- colPal[c(1, 2, 3)]

# Loading edge_list as read/run by methods
load(file.path(dataDir, "edge_list_R.RData"))

# Loading edgeList (file with some metadata)
load(file.path(dataDir, "edgeList.RData"))

# Loading method results
load(file.path(saveDir, "results_ccme.RData"))
ccme_results <- results
rm(results)
ccme_stats <- get_statistics(ccme_results)

oslom_results <- osRead(file.path(saveDir,
                                  "OSLOM2",
                                  "enron-network.dat_oslo_files",
                                  "tp"))
oslom_stats <- get_statistics(oslom_results)

slpa_results <- slpaRead(file.path(saveDir, 
                                   "SLPAw_enron-network_run1_r0.1_v3_T100.icpm"))
slpa_stats <- get_statistics(slpa_results)

# Making statistic table
stats_df <- data.frame("Num.Comms" = c(ccme_stats$K,
                                         oslom_stats$K,
                                         slpa_stats$K),
                       "Min.size" = c(min(ccme_stats$comm_sizes),
                                           min(oslom_stats$comm_sizes),
                                           min(slpa_stats$comm_sizes)),
                       "Med.size" = c(median(ccme_stats$comm_sizes),
                                           median(oslom_stats$comm_sizes),
                                           median(slpa_stats$comm_sizes)),
                       "Max.size" = c(max(ccme_stats$comm_sizes),
                                           max(oslom_stats$comm_sizes),
                                           max(slpa_stats$comm_sizes)),
                       "Num.Nodes" = c(length(ccme_stats$comm_nodes),
                                         length(oslom_stats$comm_nodes),
                                         length(slpa_stats$comm_nodes)))
stats_df2 <- data.frame("Num.OL.Nodes" = c(ccme_stats$on,
                                           oslom_stats$on,
                                           slpa_stats$on),
                       "Min.mships" = c(min(ccme_stats$om),
                                        min(oslom_stats$om),
                                        min(slpa_stats$om)),
                       "Med.mships" = c(median(ccme_stats$om),
                                        median(oslom_stats$om),
                                        median(slpa_stats$om)),
                       "Max.mships" = c(max(ccme_stats$om),
                                        max(oslom_stats$om),
                                        max(slpa_stats$om)))
rownames(stats_df) <- c("CCME", "OSLOM", "SLPAw")
rownames(stats_df2) <- c("CCME", "OSLOM", "SLPAw")
                       


# Looking at domain names
get_domain <- function (str) {
  at_pos <- gregexpr("@", str, fixed = TRUE)
  at_pos <- at_pos[[1]][1]
  domain <- substr(str, at_pos + 1, nchar(str))
  domain <- gsub(">", "", domain)
}

domains <- sapply(nodes, get_domain)
domains <- unname(domains)

ccme_domains <- table(domains[unlist(ccme_results$communities)])
oslom_domains <- table(domains[unlist(oslom_results$communities)])
slpa_domains <- table(domains[unlist(slpa_results$communities)])
pop_domains <- table(domains)

ccme_props <- sort(ccme_domains / length(unlist(ccme_results$communities)),
                   decreasing = TRUE)
ccme_prop_df <- data.frame("CCME Domains" = names(head(ccme_props)),
                           "Prop." = head(ccme_props))
oslom_props <- sort(oslom_domains / length(unlist(oslom_results$communities)),
                    decreasing = TRUE)
oslom_prop_df <- data.frame("OSLOM Domains" = names(head(oslom_props)),
                            "Prop." = head(oslom_props))
slpa_props <- sort(slpa_domains / length(unlist(slpa_results$communities)),
                   decreasing = TRUE)
slpa_prop_df <- data.frame("SLPAw Domains" = names(head(slpa_props)),
                           "Prop." = head(slpa_props))

sink(file.path(saveDir, "ccme_prop_table.txt"))
print(xtable(ccme_prop_df, digits = 3), include.rownames = FALSE)
sink()

sink(file.path(saveDir, "oslom_prop_table.txt"))
print(xtable(oslom_prop_df, digits = 3), include.rownames = FALSE)
sink()

sink(file.path(saveDir, "slpa_prop_table.txt"))
print(xtable(slpa_prop_df, digits = 3), include.rownames = FALSE)
sink()

sink(file.path(saveDir, "overall_table.txt"))
xtable(stats_df, digits = 0)
sink()

sink(file.path(saveDir, "overall_table2.txt"))
xtable(stats_df2, digits = 0)
sink()

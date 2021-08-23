
<!-- README.md is generated from README.Rmd. Please edit that file -->
# seqClustR

<!-- badges: start -->
<!-- badges: end -->
seqClustR is a sequence clustering package: it provides access to different clustering algorithms to perform sequence clustering on one common data format.

Related paper: [seqClustR: An R Package for Sequence Clustering](https://www.researchgate.net/profile/Aditya-Sharma-26/publication/353040940_seqClustR_An_R_Package_for_Sequence_Clustering/links/60e5a626299bf1b0319c7402/seqClustR-An-R-Package-for-Sequence-Clustering.pdf)

# Overview

Sequence clustering is a data mining technique that groups similar sequences into clusters based on their similarities. Sequence clustering is useful when there are unknown number of similar sequences that need to be identified to gain valuable insights.

seqClustR package provides the means to perform different clustering algorithms on sequence data by reducing the complexity to prepare data for each algorithm in a different way, by just converting the sequence data into event logs you can run multiple clustering algorithms and compare them.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("aditya9352/seqClustR")
```

## Usage

The package uses [event log](https://www.bupar.net/creating_eventlogs.html) as an input for the data. Event log are very commonly used to store the user behavior data. They indicate the sequence of actions a user takes over time, along with added metadata of the event.

Once the data is prepared in the event log format, we can run one of the following sequence clustering algorithms:

1.  **Edit Distance Clustering** - *seq\_edit\_distance\_clustering*
2.  **Markov Model Based Clustering** - *seq\_markov\_clustering*
3.  **Dynamic Time Warping** - *seq\_dtw\_clustering*
4.  **K-Means Clustering** - *seq\_kmeans\_clustering*

The output of the function would be a list containing the fitted model and a data frame having the case to cluster assigned mapping. To do further analysis on individual clusters, we need event logs for each cluster for which we have written a function *split\_event\_log* which takes event logs and the clusters assigned data frame as inputs, and returns a list of event log by cluster.

``` r
library(tidyverse)
library(bupaR)
library(seqClustR)

event_log <- sequence_data %>% 
 arrange(EventTime) %>% 
 mutate(lifecycle_id = 'complete',
        resource = NA,
        row_num = 1:nrow(.)) %>% 
 eventlog(case_id = "learnerID", 
          activity_id = "Observable", 
          activity_instance_id = "row_num",
          lifecycle_id = "lifecycle_id",
          timestamp = "EventTime", 
          resource_id = "resource")

cluster <- seq_edit_distance_clustering(
           event_log)

# Get event log by cluster as a list.

event_log_2 <- split_event_log(eventlog, 
                               cluster$cluster_assignment)
```

You can visualize the clusters using [fuzzymineR](https://github.com/nirmalpatel/fuzzymineR) package.

``` r

library(fuzzymineR)

# Process Model for Cluster 1

metrics <- mine_fuzzy_model(event_log_2[["1"]])

viz_fuzzy_model(metrics = metrics,
                node_sig_threshold = 0.1,
                edge_sig_threshold = 0.3,
                edge_sig_to_corr_ratio = 1)
```

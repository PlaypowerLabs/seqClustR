#' @title Create clusters using Edit Distance
#'
#' @description Create clusters using Edit Distance metric and Hierarchical
#'   Clustering by providing an event log object.
#'
#' @param eventlog An event log object containing case, activity and timestamp
#'   classifiers.
#' @inheritParams stringdist::seq_distmatrix
#'
#' @return A list containing the clusters assigned to each case and the fitted
#'   Hierarchical Clustering model object.
#'
#' @seealso \link[bupaR]{eventlog}, \link[stringdist]{seq_distmatrix}, \link[stats]{hclust}
#'
#' @examples
#' \dontrun{
#' library(seqClustR)
#' eventlog <- bupaR::activities_to_eventlog(data_frame,
#'                        case_id,
#'                        activity_id,
#'                        timestamp)
#' seq_edit_distance_clustering(eventlog)
#' }
#' @import dplyr
#' @import stringdist
#' @export
seq_edit_distance_clustering <- function(eventlog, method, weight, q, p, bt,
                                         nthread){

  df <- as_tibble(eventlog) %>%
    rename(case_id = attributes(eventlog)$case_id,
           timestamp = attributes(eventlog)$timestamp,
           activity_id = attributes(eventlog)$activity_id) %>%
    select(case_id, timestamp, activity_id) %>%
    unique() %>%
    arrange(case_id, timestamp)

  intify <- function(x) {
    y <- sort(unique(x))
    z <- setNames(1:length(y), y)
    z[x]
  }

  df[["activity_id"]] <- intify(df[["activity_id"]])

  # encoded trace list
  trace_list <- lapply(split(df, df[["case_id"]]), function(case_data) case_data[["activity_id"]])

  trace_distances <- seq_distmatrix(trace_list)

  hc_mod <- hclust(trace_distances, method = "ward.D")

  plot(hc_mod, labels = FALSE, main = "", xlab = "", sub = "")

  cut_by <- NULL

  while(!cut_by %in% c('1', '2')){
  cut_by <- readline(prompt = "Please select an option:\n1. Cut by number of groups\n2. Cut by height")
  }

  cluster_eventlog <- function(x, m, ...) {

    stopifnot(x %hascols% c("caseid", "activity", "completeTime"))

    stopifnot(class(m) == "hclust")

    # cut tree at a specific place to generate cluster assignments
    clusters <- cutree(m, ...)

    # make log with cluster information
    x %>%
      mutate_(cluster = ~ unname(clusters[as.character(case_id)]))
  }

  if(cut_by == 1){
    n_groups <- NULL
    while(!is.integer(n_groups) && n_groups <= 1){
    n_groups <- readline(prompt = "Please enter the desired number of groups.")
    }

    cluster_df <- cluster_eventlog(df, hc_mod, k = n_groups) %>%
      select(case_id, cluster) %>%
      unique()
  }
  else if(cut_by == 2){
    height <- NULL
    while(!is.numeric(height)){
    height <- readline(prompt = "Please enter the height where the tree should be cut.")
    }

    cluster_df <- cluster_eventlog(df, hc_mod, h = height) %>%
      select(case_id, cluster) %>%
      unique()
  }

  my_list <- list("model" = hc_mod, "cluster_assignment" = cluster_df)
  return(my_list)
}

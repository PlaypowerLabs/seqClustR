#' @title Create clusters using K-Means Method
#'
#' @description Create clusters using K-Means Method by providing an event log
#'   object.
#'
#' @param eventlog An event log object containing case, activity and timestamp
#'   classifiers.
#' @inheritParams stats::kmeans
#'
#' @return A list containing the clusters assigned to each case and the fitted
#'   K-Means model.
#'
#' @seealso \link[bupaR]{eventlog}, \link[stats]{kmeans}
#'
#' @examples
#' \dontrun{
#' library(seqClustR)
#' eventlog <- bupaR::activities_to_eventlog(data_frame,
#'                        case_id,
#'                        activity_id,
#'                        timestamp)
#' seq_kmeans_clustering(eventlog)
#' }
#' @import dplyr
#' @export
seq_kmeans_clustering <- function(eventlog, centers = 2, iter.max = 10, nstart = 1,
                                  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                                "MacQueen"), trace = FALSE){

  df <- as_tibble(eventlog) %>%
    rename(case_id = attributes(eventlog)$case_id,
           timestamp = attributes(eventlog)$timestamp,
           activity_id = attributes(eventlog)$activity_id) %>%
    group_by(case_id, activity_id) %>%
    summarise(n_instances = n()) %>%
    ungroup() %>%
    group_by(case_id) %>%
    mutate(percentage_instances = n_instances/sum(n_instances)*100) %>%
    ungroup() %>%
    select(-n_instances) %>%
    spread(activity_id, percentage_instances, fill = 0)

  km_mat <- as.matrix(df[, 3:ncol(df)])

  km_mod <- kmeans(x = km_mat, centers, iter.max, nstart, algorithm, trace)

  cluster_df <- df %>%
    select(case_id) %>%
    mutate(cluster = km_mod$cluster)

  my_list <- list("model" = km_mod, "cluster_assignment" = cluster_df)
  return(my_list)

}

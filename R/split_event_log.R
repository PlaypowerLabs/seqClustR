#' @title Get event logs for each cluster
#'
#' @description This function helps in getting event logs for each cluster which
#'   can then be used to create process models for clusters and visualize them
#'   as well using the fuzzymineR package or by using some other tools like
#'   ProM or Disco.
#'
#' @param eventlog An event log object containing case, activity and timestamp
#'   classifiers.
#' @param cluster_assignment A data frame containing the clusters assigned
#'   for each case. Output of seq_edit_distance_clustering,
#'   seq_kmeans_clustering, seq_markov_clustering or seq_dtw_clustering can be
#'   passed here.
#'
#' @return A list containing event logs for each cluster.
#'
#' @seealso \link[bupaR]{eventlog}, \link[bupaR]{activities_to_eventlog}
#'
#' @examples
#' \dontrun{
#' library(seqClustR)
#' eventlog <- bupaR::activities_to_eventlog(data_frame,
#'                        case_id,
#'                        activity_id,
#'                        timestamp)
#' split_event_log(eventlog, cluster_assignment)
#' }
#' @import dplyr
#' @export
split_event_log <- function(eventlog, cluster_assignment) {

  colnames(cluster_assignment) <- c("case_id", "cluster")

  eventlog_df <- as_tibble(eventlog) %>%
    dplyr::rename(case_id = attributes(eventlog)$case_id,
                  activity_id = attributes(eventlog)$activity_id,
                  activity_instance_id = attributes(eventlog)$activity_instance_id,
                  lifecycle_id = attributes(eventlog)$lifecycle_id,
                  timestamp = attributes(eventlog)$timestamp,
                  resource_id = attributes(eventlog)$resource_id
    ) %>%
    dplyr::select(case_id, activity_id, activity_instance_id, lifecycle_id, timestamp, resource_id) %>%
    unique() %>%
    dplyr::inner_join(cluster_assignment, by = "case_id")

  clusters <- sort(unique(eventlog_df$cluster))

  log_list <- lapply(clusters, function(x) {

    eventlog_df %>%
      dplyr::filter(cluster == x) %>%
      eventlog(case_id = "case_id",
               activity_id = "activity_id",
               activity_instance_id = "activity_instance_id",
               lifecycle_id = "lifecycle_id",
               timestamp = "timestamp",
               resource_id = "resource_id")
  })

  names(log_list) <- clusters

  return(log_list)

  # colnames(cluster_assignment) <- c("case_id", "cluster")
  #
  # eventlog_df <- as_tibble(eventlog) %>%
  #   dplyr::rename(case_id = attributes(eventlog)$case_id,
  #                 timestamp = attributes(eventlog)$timestamp,
  #                 activity_id = attributes(eventlog)$activity_id) %>%
  #   dplyr::select(case_id, timestamp, activity_id) %>%
  #   unique() %>%
  #   dplyr::inner_join(cluster_assignment, by = "case_id")
  #
  # clusters <- sort(unique(eventlog_df$cluster))
  #
  # log_list <- lapply(clusters, function(x) {
  #
  #   filtered_eventlog_df <- eventlog_df %>%
  #     dplyr::filter(cluster == x)
  #
  #   bupaR::activities_to_eventlog(filtered_eventlog_df,
  #                               case_id = "case_id",
  #                               activity_id = "activity_id",
  #                               timestamp = "timestamp")
  #
  # })
  #
  # names(log_list) <- clusters
  #
  # return(log_list)
}

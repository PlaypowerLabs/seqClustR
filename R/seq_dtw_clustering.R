#' @title Create clusters using DTW
#'
#' @description Create clusters using Dynamic Time Warping (DTW) by providing
#'   an event log object.
#'
#' @param eventlog An event log object containing case, activity and timestamp
#'   classifiers.
#' @inheritParams dtwclust::tsclust
#'
#' @return A list containing the clusters assigned to each case and an object
#'   with an appropriate class from \link[dtwclust]{TSClusters}.
#'
#' @seealso \link[bupaR]{eventlog}, \link[dtwclust]{tsclust}
#'
#' @examples
#' \dontrun{
#' library(seqClustR)
#' eventlog <- bupaR::activities_to_eventlog(data_frame,
#'                        case_id,
#'                        activity_id,
#'                        timestamp)
#' seq_dtw_clustering(eventlog)
#' }
#' @import dplyr
#' @import dtwclust
#' @export
seq_dtw_clustering <- function(eventlog, type, k, ..., preproc, distance,
                               centroid, control, args, seed, trace,
                               error.check){

  df <- dplyr::as_tibble(eventlog) %>%
    dplyr::rename(case_id = attributes(eventlog)$case_id, timestamp = attributes(eventlog)$timestamp,
           activity_id = attributes(eventlog)$activity_id) %>%
    dplyr::select(case_id, timestamp, activity_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(case_id, timestamp)

  action_actionid_mapping <- df %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(actionid = row_number()) %>%
    dplyr::select(activity_id, actionid)

  # joining the obtained action ids to the original clickstream data

  df_2 <- df %>%
    dplyr::inner_join(action_actionid_mapping)

  # getting a sequence of actionids for each session

  ts_df <- df_2 %>%
    dplyr::group_by(case_id) %>%
    dplyr::summarise(seq_data = paste(actionid, collapse = ","))

  # converting the sequences obtained into a list

  ts_list_data <- dtwclust::tslist(as.data.frame(ts_df$seq_data))

  # splitting by comma

  ts_list_data_new <- lapply(ts_list_data, function(x){ as.numeric(strsplit(x,",")[[1]])})

  # model

  dtw_model <- dtwclust::tsclust(ts_list_data_new, ...)

  ts_df$cluster <- attr(dtw_model, "cluster")

  # getting cluster mapping

  cluster_df <- ts_df %>%
    dplyr::select(case_id, cluster)

  my_list <- list("model" = dtw_model, "cluster_assignment" = cluster_df)
  return(my_list)

}

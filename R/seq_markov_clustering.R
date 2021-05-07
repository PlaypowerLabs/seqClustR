#' @title Create clusters using Markov Model
#'
#' @description Create clusters using Markov Model by providing an event log
#'   object.
#'
#' @param eventlog An event log object containing case, activity and timestamp
#'   classifiers.
#' @inheritParams seqHMM::seqdef
#' @inheritParams seqHMM::build_mmm
#' @inheritParams seqHMM::fit_model
#'
#' @return A list containing the clusters assigned to each case and the fitted
#'   Markov Model.
#'
#' @seealso \link[bupaR]{eventlog}, \link[seqHMM]{seqdef}, \link[seqHMM]{build_mmm}, \link[seqHMM]{fit_model}
#'
#' @examples
#' \dontrun{
#' library(seqClustR)
#' eventlog <- bupaR::activities_to_eventlog(data_frame,
#'                        case_id,
#'                        activity_id,
#'                        timestamp)
#' seq_markov_clustering(eventlog)
#' }
#' @import dplyr
#' @import seqHMM
#' @export
seq_markov_clustering <- function(eventlog, informat, alphabet, states, id, weights, start,
                                  left, right, gaps, missing, void, nr,
                                  cnames, xtstep, tick.last, cpal,
                                  missing.color, labels, n_clusters, transition_probs, initial_probs,
                                  formula, data, coefficients, cluster_names, em_step, global_step,
                                  local_step, control_em, control_global,
                                  control_local, lb, ub, threads, log_space, ...){

  df <- as_tibble(eventlog) %>%
    rename(case_id = attributes(eventlog)$case_id,
           timestamp = attributes(eventlog)$timestamp,
           activity_id = attributes(eventlog)$activity_id) %>%
    select(case_id, timestamp, activity_id) %>%
    unique() %>%
    arrange(case_id, timestamp) %>%
    group_by(case_id) %>%
    summarise(seqdat = paste0(activity_id, collapse = ",")) %>%
    ungroup()

  # building sequences

  seqs <- seqHMM::seqdef(as.data.frame(df), var = 2, stsep = ",", ...)

  mod <- seqHMM::build_mmm(seqs, n_clusters, ...)

  # fitting model

  fit_global <- seqHMM::fit_model(mod, ...)

  cluster_df<- df %>%
    mutate(cluster = as.character(summary(fit_global$model)$most_probable_cluster)) %>%
    select(case_id, cluster)

  my_list <- list("model" = fit_global, "cluster_assignment" = cluster_df)
  return(my_list)

}

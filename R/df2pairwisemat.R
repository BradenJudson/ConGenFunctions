#' @title Dataframe to pairwise matrix converter
#' @description
#' Converts a long-form dataframe containing pairwise values into a symmetrical matrix.
#'
#' @param df A long-form dataframe containing two columns of grouping variables and a column of values.
#' @param var Column name containing the value of interest (to populate the matrix).
#' @param grp1 Grouping variable column 1 (e.g., population).
#' @param grp2 Grouping variable column 2 (e.g., population).
#'
#' @return Distance matrix of the class Matrix.
#' @import rlang
#' @importFrom stats xtabs
#' @importFrom stats as.formula
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom magrittr '%>%'
#' @importFrom rlang .data
#' @export

df2pmat <- \(df, var, grp1, grp2) {

  df_mat <- as.data.frame.matrix(stats::xtabs(as.formula(paste(var, "~", grp1, "+", grp2)), data = df))
  mrow   <- colnames(df_mat)[!colnames(df_mat) %in% rownames(df_mat)]
  mcol   <- rownames(df_mat)[!rownames(df_mat) %in% colnames(df_mat)]

  adjmat <- rbind(as.data.frame(matrix(data = 0,
                                       nrow = 1,
                                       ncol = ncol(df_mat) + 1,
                                       dimnames = list(c(mrow), c(colnames(df_mat), mcol))),
                                       row.names = mrow),
                  df_mat %>% mutate({{mcol}} := 0))

  # Make it so the matrix is symmetrical across the diagonal.
  adjmat[upper.tri(adjmat)] <- t(adjmat)[upper.tri(adjmat)]
  diag(adjmat) <- NA # Make the diagonal NAs instead of 0s.
  return(adjmat)

}

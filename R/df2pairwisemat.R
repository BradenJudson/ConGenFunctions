#' @title Dataframe to pairwise matrix converter
#' @description
#' Converts a long-form dataframe containing pairwise values into a symmetrical matrix.
#'
#' @param df A long-form dataframe containing two columns of grouping variables and a column of values.
#' @param var Column name containing the value of interest (to populate the matrix).
#' @param grp1 Grouping variable column 1 (e.g., population).
#' @param grp2 Grouping variable column 2 (e.g., population).
#' @param diag Value to fill in the diagonal (default is 0).
#'
#' @return Distance matrix of the class Matrix.
#' @import rlang
#' @importFrom stats as.formula
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export

df2pmat <- \(df, var, grp1, grp2, diag) {

  matrix_names <- sort(unique(as.character(unlist(df[c(grp1, grp2)]))))

  mat <- matrix(0, length(matrix_names), length(matrix_names),
                dimnames = list(matrix_names, matrix_names))

  mat[as.matrix(df[c(grp1, grp2)])] <- df[[var]]
  mat[as.matrix(df[c(grp2, grp1)])] <- df[[var]]

  diag <- if (missing(diag)) { 0 } else { diag }

  diag(mat) <- diag
  return(mat)
}

#' @title Read *.frq file from VCFtools
#' @description
#' A function for reading in and formatting the output of VCFtools --freq --derived into a tidy data frame.
#'
#' @param file File path to a *.frq file produced by VCFtools. Column labels assume that files were generated with the flag --derived.
#'
#' @import dplyr
#' @importFrom readr read_table
#' @return
#' @export
#'

read_frq <- \(file) {
  dat <- read_table(file,
                    col_names = c("CHROM",
                                  "POS", "N_ALLELES",
                                  "N_CHR", "Ancestral",
                                  "Alternative")) %>%
    filter(Ancestral != "{ALLELE:FREQ}") %>%
    mutate(MajAF  = as.numeric(sub(".*:", "", Ancestral)),
           MajNuc = as.factor(substr(Ancestral, start = 0, stop = 1)),
           MinAF  = as.numeric(sub(".*:", "", Alternative)),
           MinNuc = as.factor(substr(Alternative, start = 0, stop = 1))) %>%
    dplyr::select(!c("Ancestral", "Alternative", "N_CHR", "N_ALLELES"))

}

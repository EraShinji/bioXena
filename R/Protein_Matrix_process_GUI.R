
#' Protein_Matrix_process_GUI
#'
#' @param tar_gene
#'
#' @return Return Processed Protein Data(By Tibble)
#' @export
#'
#' @examples
Protein_Matrix_process_GUI <- function(tar_gene){
  library("tidyverse")
  path <- file.choose()
  Protein_matrix_Origin <- read_tsv(path,col_names = TRUE) %>%
    rename(Sample=sample) %>%
    filter(Sample %in% tar_gene)
  Protein_matrix_PreConvert <- Protein_matrix_Origin %>%
    pivot_longer(cols = 2:19132,
                 names_to = "sample",
                 values_to = "Protein Expose") %>%
    group_by(Sample) %>%
    pivot_wider(names_from = Sample,
                values_from = "Protein Expose")%>%
    rename(Sample=sample)
  return(Protein_matrix_PreConvert)
}

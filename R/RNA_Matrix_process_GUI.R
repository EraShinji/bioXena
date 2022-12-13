#' Title
#'
#' @param tar_gene T
#'
#' @return RNAmatrix_PreConvert
#' @export
#'
#' @examples
RNA_Matrix_process_GUI <- function(tar_gene){
  library("tidyverse")
  path <- file.choose()
  RNAmatrix_Origin <- read_tsv(path,col_names = TRUE) %>%
    rename(Sample=sample) %>%
    filter(Sample %in% tar_gene)
  RNAmatrix_PreConvert <- RNAmatrix_Origin %>%
    pivot_longer(cols = 2:dim(RNAmatrix_Origin)[2],
                 names_to = "sample",
                 values_to = "expose") %>%
    group_by(Sample) %>%
    pivot_wider(names_from = Sample,
                values_from = "expose")%>%
    rename(Sample=sample)
  return(RNAmatrix_PreConvert)
}

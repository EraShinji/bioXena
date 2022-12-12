
#' Title
#'
#' @param tar_gene
#'
#' @return
#' @export
#'
#' @examples
CNA_Matrix_process <- function(tar_gene){
  library("tidyverse")
  path <- file.choose()
  CNAmatrix_Origin <- read_tsv(path,col_names = TRUE) %>%
    filter(Sample %in% tar_gene)
  CNAmatrix_PreConvert <- CNAmatrix_Origin %>%
    pivot_longer(cols = 2:dim(CNAmatrix_Origin)[2],
                 names_to = "sample",
                 values_to = "CNA") %>%
    group_by(Sample) %>%
    pivot_wider(names_from = Sample,
                values_from = "CNA")%>%
    rename(Sample=sample)
  return(CNAmatrix_PreConvert)
}


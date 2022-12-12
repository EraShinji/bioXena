#' Title
#'
#' @param tar_gene The Gene Name needed to be filtered
#' @param target_path The Dataset Filepath
#'
#' @return
#' @export
#'
RNA_Matrix_process_CLI <- function(tar_gene,target_path){
  library("tidyverse")
  library(magrittr)
  path <- temp_path
  RNAmatrix_Origin <- read_tsv(path,col_names = TRUE) %>%
    select(Sample = 1,everything()) %>%
    mutate(Sample = str_sub(.$Sample,start = 1,end = 15)) %>%
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

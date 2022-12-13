
#' CNA_Matrix_process_GUI
#'
#' @param tar_gene
#'
#' @return Return Processed CNA Datas(By tibble)
#' @export
#'
#' @examples
CNA_Matrix_process_GUI <- function(tar_gene){
  library("tidyverse")
  path <- file.choose()
  CNAmatrix_Origin <- read_tsv(path,col_names = TRUE) %>%
    select(Sample = 1,everything()) %>%
    mutate(Sample = str_sub(.$Sample,start = 1,end = 15)) %>%
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


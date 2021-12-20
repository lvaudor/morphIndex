#' Calculate morphological indexes multiple rivers or reaches
#' @param Infos a table containing river-related details.
#' @return a table of morphological indexes
#' @export
braidIndex=function(Infos,rel_path=""){
  result=Infos %>%
    mutate(File=paste0(rel_path,File)) %>%
    mutate(data=purrr::map(.x=File,
                           .f=braidIndex_read)) %>%
    mutate(data=purrr::pmap(list(rivdata=data,
                                 area=Area,
                                 points_space=Points_space),
                            .f=braidIndex_one)) %>%
    tidyr::unnest(cols=data)
  return(result)
}

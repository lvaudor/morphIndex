#' Read data to
#'
#' @param File must have ";" as column separators and extension ".txt". Must have columns "ID","RASTERVALU","Z","TYPO_VEGE","NAME".
#' @return
#' @export
#' @examples
#' braidIndex_read("data-raw/Drac_Chabottes_2018.txt")
braidIndex_read=function(File){
  rivdata=read.csv(File, row.names=1,sep=";") %>%
    rename(ID_XS=ID,
           Z=RASTERVALU) %>%
    filter(Z!=-9999) %>%
    filter(TYPO_VEGE != "mature" & NAME != "Riparian") %>%
    mutate_at(.vars = c("NAME", "TYPO_VEGE"), .funs = as.factor) %>%
    return(rivdata)
}

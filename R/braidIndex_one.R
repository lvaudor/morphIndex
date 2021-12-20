#' Calculate morphological indexes for one river or reach
#' @param rivdata river data
#' @param area total area
#' @param points_space space between points
#' @return a 1-row table of morphological indexes
#' @export
braidIndex_one=function(rivdata, area, points_space){
  rivdata_comp=rivdata %>%
    dplyr::group_by(ID_XS) %>%
    dplyr::mutate(Mean_XS=mean(Z),
           Zdiff=(Z - Mean_XS) ^ 2) %>%
    dplyr::ungroup()
  tib_XS <- rivdata_comp %>%
    dplyr::group_by(ID_XS) %>%
    dplyr::summarise(Nb_measure=dplyr::n(),
                     Mean_XS=mean(Z),
                     Sum_Zdiff = sum(Zdiff),
                     Numerator=(((1 / (Nb_measure - 1)) * Sum_Zdiff) ^ 0.5),
                     BRI=Numerator / ((Nb_measure - 1) * points_space),
                     WAC=(Nb_measure - 1) * points_space,
                     W=WAC / (area ^ 0.44)) %>%
    dplyr::mutate(BRI_norm=dplyr::case_when((Nb_measure==1)~NA_real_,
                                            !(Nb_measure==1)~BRI))
  only_one_measure=tib_XS %>%
    dplyr::filter(Nb_measure==1)
  if(nrow(only_one_measure)>0){
    warning(paste0("The cross-sections with ID ",
                   paste0(only_one_measure$ID_XS, collapse="-"),
                   " contain only one measure. "))
  }
  tib_result=tib_XS %>%
    dplyr::summarise(
      Nb_mean_meas=mean(Nb_measure),
      XS_onlyone=length(which(Nb_measure==1))/dplyr::n()*100,
      BRI_mean=mean(BRI, na.rm=TRUE),
      BRI_SD=sd(BRI, na.rm=TRUE),
      BRI_min=min(BRI, na.rm=TRUE),
      BRI_max=max(BRI, na.rm=TRUE),
      WAC_mean=mean(WAC) ,
      WAC_SD=sd(WAC),
      WAC_min=min(WAC),
      WAC_max=max(WAC),
      W_mean=mean(W)  ,
      W_SD=sd(W),
      W_min=min(W),
      W_max=max(W))
  tib_result=tidyr::pivot_longer(tib_result,
                                 cols=c(starts_with("BRI_"),
                                        starts_with("W_"),
                                        starts_with("WAC_")),
                           names_to="variable",
                           values_to="value") %>%
    tidyr::separate(variable,sep="_",into=c("type","stat"),remove=FALSE)
  if(any(tib_result$XS_onlyone>20)){
    warning(paste0("There are more than 20% of the data containing a unique measure on cross section,',
                   'you should review your data.")
    )
  }
  return(tib_result)
}

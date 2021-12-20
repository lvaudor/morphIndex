#' Plot index (either W`*` or BRI`*`)
#' @param tib_morpho_index the table with morphological indexes values, resulting from a call to braidIndex().
#' @param position the variable according to which estimates should be placed on graphic.
#' @param color the variable according to which estimates should be colored.
#' @param threshold the threshold for a section to be considered as braiding (defaults to 0.004).
#' @return plot of index
#' @export
braidIndex_plot=function(tib_morpho_index,index="W",position,color, threshold=0.004){
  tib_plot=tib_morpho_index %>%
    dplyr::filter(type==index) %>%
    dplyr::select(-variable) %>%
    tidyr::pivot_wider(names_from="stat",
                       values_from="value") %>%
    dplyr::mutate(IC_min=mean-SD,
                  IC_max=mean+SD)
  color=enquo(color)
  position=enquo(position)
  p<- ggplot2::ggplot(data = tib_plot,
             ggplot2::aes(x = !!position, y =mean , col = !!color)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.004,
                                     linetype = "Braiding threshold \n(braiding < 0.004 < not braiding)"), colour = 'black') +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = IC_min, ymax = IC_max), width = 0.1, size = 0.5) +
    ggplot2::scale_linetype_manual(name = NULL, values = 4) +
    ggplot2::scale_y_continuous(paste0(index,"*"),
                                limits = c(min(tib_plot$IC_min),
                                           max(tib_plot$IC_max)))
  return(p)
}


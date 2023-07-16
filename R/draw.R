#' @rdname getMoonplot
#' @export
getMoonplot = function(result, ...){
  UseMethod("getMoonplot", result)
}

#' Get a moonplot from MCA
#'
#' @rdname getMoonplot
#' @param result The result of the correspondence analysis or multiple correspondence analysis.s
#' @param rextra Space between the outermost point and the edges of the moon.
#' @param textmax Size of the halo around the moon reserved for category names.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
getMoonplot.MCA = function(result, rextra = .1, textmax = .2){
  norms_ind = sqrt((res.mca$ind$coord[,1])^2 + (res.mca$ind$coord[,2])^2)
  theta_ind = atan(res.mca$ind$coord[,1] / res.mca$ind$coord[,2]) + (res.mca$ind$coord[,2] < 0) * pi + (res.mca$ind$coord[,2] > 0 & res.mca$ind$coord[,1] < 0) * 2 * pi
  quadrant = c(case_when(
    res.mca$ind$coord[, 1] > 0 & res.mca$ind$coord[, 2] > 0 ~ 1L,
    res.mca$ind$coord[, 1] > 0 & res.mca$ind$coord[, 2] < 0 ~ 2L,
    res.mca$ind$coord[, 1] < 0 & res.mca$ind$coord[, 2] < 0 ~ 3L,
    T ~ 4L
  ))

  norms_var = sqrt((res.mca$var$coord[,1])^2 + (res.mca$var$coord[,2])^2)
  theta_var = atan(res.mca$var$coord[,1] / res.mca$var$coord[,2]) + (res.mca$var$coord[,2] < 0) * pi + (res.mca$var$coord[,2] > 0 & res.mca$var$coord[,1] < 0) * 2 * pi


  ind_df = data.frame(theta_ind, norms_ind)
  var_df = data.frame(name = rownames(res.mca$var$coord), theta_var, norms_var)

  plot_inds = ggplot(ind_df, aes(x = theta_ind, y = norms_ind)) +
    geom_point() +
    scale_colour_discrete() +
    scale_x_continuous(limits = c(0, 2 * pi), expand = c(0, 0)) +
    scale_y_continuous(breaks = max(norms_ind) + rextra,
                       limits = c(0, max(norms_ind) + rextra + textmax))+
    coord_polar(theta = "x")  +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    geom_hline(yintercept = max(norms_ind) + rextra)

  plot_full = plot_inds +
    geom_text(data = var_df, aes(x = theta_var, alpha = norms_var,  size = norms_var, label = name, angle = (pi/2 - theta_var) / pi * 180), y = max(norms_ind) + rextra, vjust = "outward", hjust = "outward")
  plot_full
}

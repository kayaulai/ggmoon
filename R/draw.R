tilt = function(theta, flip = F){
  if(!flip){
    (pi/2 - theta) / pi * 180
  } else {
    sapply(theta, function(x){
      if(x < pi){
        (pi/2 - x) / pi * 180
      } else {
        (pi/2 - x) / pi * 180 + 180
      }
    })
  }
}

determineDir = function(theta){
  sapply(theta, function(x){
    if(x < pi){
      "left"
    } else {
      "right"
    }
  })
}

determineNudge = function(theta){
  sapply(theta, function(x){
    if(x < pi){
      .2
    } else {
      -.2
    }
  })
}



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
#' @param flip Flip the text on the left side of the plot?
#' @param row_col A vector of the same size as the number of rows / individuals, mapping onto the colour of the row/individual points.
#' @param col_col A vector of the same size as the number of columns (CA) / variables (MCA), mapping onto the colour of the column/variable points.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples data(tea)
#' res.mca = MCA(tea, quanti.sup=19, quali.sup=20:36, graph = F)
#' getMoonplot(res.mca, flip = T, row_col = tea[,20])

getMoonplot.MCA = function(result, rextra = .1, textmax = .2, nudge_x_var = .3, flip = F, ind_col = NULL, var_col = NULL){
  getMoonplot_general(result$ind$coord, result$var$coord, rextra = rextra, textmax = textmax, nudge_x_col = nudge_x_var, flip = flip, row_col = ind_col, col_col = var_col, rotate_col_text = rotate_var_text)
}

#' @rdname getMoonplot
#' @export
#' @examples data(children)
#' res.ca = CA(children, row.sup = 15:18, col.sup = 6:8, graph = F)
#' getMoonplot(res.ca, flip = T)
getMoonplot.CA = function(result, rextra = .1, textmax = .2, nudge_x_col = .3, flip = F, label_rows = F, row_col = NULL, col_col = NULL){
  getMoonplot_general(result$row$coord, result$col$coord, rextra = rextra, textmax = textmax, flip = flip, label_rows = label_rows, row_col = row_col, col_col = col_col)
}

getMoonplot_general = function(row_coords, col_coords, rextra = .1, textmax = .2, nudge_x_col = .3, flip = F, label_rows = F, row_col = NULL, col_col = NULL, rotate_col_text = T){
  norms_row = sqrt((row_coords[,1])^2 + (row_coords[,2])^2)
  theta_row = atan(row_coords[,1] / row_coords[,2]) + (row_coords[,2] < 0) * pi + (row_coords[,2] > 0 & row_coords[,1] < 0) * 2 * pi
  quadrant = c(case_when(
    row_coords[, 1] > 0 & row_coords[, 2] > 0 ~ 1L,
    row_coords[, 1] > 0 & row_coords[, 2] < 0 ~ 2L,
    row_coords[, 1] < 0 & row_coords[, 2] < 0 ~ 3L,
    T ~ 4L
  ))

  norms_col = sqrt((col_coords[,1])^2 + (col_coords[,2])^2)
  theta_col = atan(col_coords[,1] / col_coords[,2]) + (col_coords[,2] < 0) * pi + (col_coords[,2] > 0 & col_coords[,1] < 0) * 2 * pi


  row_df = data.frame(name = rownames(row_coords), theta_row, norms_row)
  col_df = data.frame(name = rownames(col_coords), theta_col, norms_col)

  point_aes = aes()
  if(!is.null(row_col)){
    point_aes = aes(col = as.factor(row_col))
  }

  plot_rows = ggplot(row_df, aes(x = theta_row, y = norms_row)) +
    geom_point(mapping = point_aes) +
    scale_colour_discrete() +
    scale_x_continuous(limits = c(0, 2 * pi), expand = c(0, 0)) +
    scale_y_continuous(breaks = max(norms_row) + rextra,
                       limits = c(0, max(norms_row) + rextra + textmax))+
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
    geom_hline(yintercept = max(norms_row) + rextra)

  if(rotate_col_text){
    plot_full = plot_rows +
      geom_text(data = col_df, aes(x = theta_col, alpha = norms_col,  size = norms_col, label = name, angle = tilt(theta_col, flip)), y = max(norms_row) + rextra, vjust = "outward", hjust = "outward") +
      scale_size(guide = "none")
  } else {
    plot_full = plot_rows +
      ggrepel::geom_text_repel(data = col_df %>% filter(theta_col > pi), aes(x = theta_col, alpha = norms_col, label = name, hjust = determineDir(theta_col)), y = max(norms_row) + rextra, vjust = "outward", direction = "y", nudge_x = -nudge_x_col, ylim = c(max(norms_row) + rextra, Inf), max.overlaps = Inf)+
      ggrepel::geom_text_repel(data = col_df %>% filter(theta_col <= pi), aes(x = theta_col, alpha = norms_col, label = name, hjust = determineDir(theta_col)), y = max(norms_row) + rextra, vjust = "outward", direction = "y", nudge_x = nudge_x_col, ylim = c(max(norms_row) + rextra, Inf), max.overlaps = Inf) +
      scale_size(guide = "none")
  }

  if(label_rows){
    text_aes = aes(x = theta_row, y = norms_row, label = name)
    if(!is.null(row_col)){
      text_aes = modifyList(text_aes, aes(col = as.factor(row_col)))
    }
    plot_full = plot_full + ggrepel::geom_text_repel(text_aes)
  }
  plot_full
}


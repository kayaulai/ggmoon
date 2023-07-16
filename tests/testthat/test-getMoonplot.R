test_that("MCA works", {
  data(tea)
  res.mca = MCA(tea, quanti.sup=19, quali.sup=20:36, graph = F)
  res.mca$ind$coord[,1:2] %>% data.frame %>% ggplot(aes(x = `Dim.1`, y = `Dim.2`)) + geom_point()
  res.mca$var$coord[,1:2] %>% data.frame %>% ggplot(aes(x = `Dim.1`, y = `Dim.2`)) + geom_point()
  getMoonplot(res.mca, flip = T, ind_col = tea[,20], nudge_x_var = .2, rotate_var_text = F)
})

test_that("CA works", {
  data(children)
  res.ca = CA(children, row.sup = 15:18, col.sup = 6:8, graph = F)
  getMoonplot(res.ca, flip = T)
})

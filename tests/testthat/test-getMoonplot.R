test_that("multiplication works", {
  data(tea)
  res.mca = MCA(tea, quanti.sup=19, quali.sup=20:36, graph = F)
  res.mca$ind$coord[,1:2] %>% data.frame %>% ggplot(aes(x = `Dim.1`, y = `Dim.2`)) + geom_point()
  res.mca$var$coord[,1:2] %>% data.frame %>% ggplot(aes(x = `Dim.1`, y = `Dim.2`)) + geom_point()
  getMoonplot(res.mca)
})

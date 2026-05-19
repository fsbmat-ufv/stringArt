rm(list=ls())
cat("\014")
#install.packages(c("devtools", "usethis", "roxygen2", "testthat"))
library(devtools)
#usethis::use_mit_license("Fernando de Souza Bastos")
# 1. Editar a função em R/
usethis::use_r("stcircle")

# 2. Carregar o pacote
devtools::load_all()

# 3. Testar manualmente
stcircle(
  n = 20,
  k = 3,
  r = 1,
  col = "blue",
  lwd = 1
)

# 4. Atualizar documentação
devtools::document()

# 5. Rodar testes
devtools::test()

# 6. Checar o pacote
devtools::check()

git add .
git commit -m "Adiciona função stcircle"
git push

# usethis::use_test("stcircle")
# devtools::test()
# usethis::use_github_links()


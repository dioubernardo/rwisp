
# Enviar ao shinyapp.io

rsconnect::deployApp('.')

# Regear a documentação

library(roxygen2)
roxygenise()

# Envio ao cran

library(devtools)

check(cran = TRUE)

build(pkg = ".")

release(pkg = ".", check = TRUE)

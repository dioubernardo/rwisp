
# Enviar ao shinyapp.io

rsconnect::deployApp('.')

# Regear a documentação

library(roxygen2)
roxygenise()

# Envio ao cran

-- tag 
git tag -a v1.0.4 -m "Version 1.0.4"
git push -u origin v1.0.4

-- build

library(devtools)

check(cran = TRUE)
build(pkg = ".")

spell_check()
check_rhub()
check_win_devel()

release(pkg = ".")

BUG do i18n
https://github.com/Appsilon/shiny.i18n/issues/123
https://community.rstudio.com/t/package-not-being-recognized-in-the-application/168165

# shiny-euros
Shinydashboard to display stats and scores from the euros 2024 for module SBD1 at BFH

Link to deployed app: https://keijo-nierula.shinyapps.io/euros-dashboard/

GitHub Repo: https://github.com/kane17/shiny-euros

Data Source: https://raw.githubusercontent.com/openfootball/euro.json/master/2024/euro.json

## How to deploy to shinyapps.io


```{R}
# 1. Create Account
# 2. Install rsconnect package:
install.packages('rsconnect')
# Login to account:
rsconnect::setAccountInfo(name='USERNAME',  token='TOKEN',  secret='SECRET')
# Deploy app:
rsconnect::deployApp('./euros-dashboard')
```
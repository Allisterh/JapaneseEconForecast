source("setup.r")

print("running baseline models")
source("baseline.r")

print("running AR")
source("ar.r") # `lagLenAR`` records the lag length selected for each window 
source("ar2.r")
source("ar3.r")
source("ar4.r")

print("running VAR, expect 2 progress bars")
source("var.r")
source("var2.r")
source("var3.r")
source("var4.r")

print("running lasso, expect 2 progress bars")
source("lasso.r")
source("lasso2.r")

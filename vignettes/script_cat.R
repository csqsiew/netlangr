# install.packages('devtools')
library(devtools)
install_github('csqsiew/netlangr')
library(netlangr)

data <- read.csv('cat.csv', stringsAsFactors = F)

# Phonological network
phono.net <- makelangnet(data$Phono, parallel = T) # make the language network 
phono.net.measures <- getnetstats(phono.net) # get network measures

# both phonology and orthography
multi.net <- makemultinet(data, parallel = T)
multi.net.measures <- getmultistats(multi.net)

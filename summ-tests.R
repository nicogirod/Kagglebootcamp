##### Combine the test scores from all validation sets  
#####
##### edit: fname
#####

fname = "map-test13.csv"

report1 <- read.csv(paste0("data/split1/", fname))
report2 <- read.csv(paste0("data/split2/", fname))
#report3 <- read.csv(paste0("data/split3/", fname))
#ave <- (report1[,10] + report2[,10] + report3[,10]) / 3
ave <- (report1[,10] + report2[,10]) / 2

report <- cbind(report1, report2[,10], ave)
colnames(report) <- c(paste0("wt", 1:9),
                      "validation1",
                      "validation2",
                      "average")

write.csv(report, paste0("data/splits-summ/", fname), row.names=FALSE)


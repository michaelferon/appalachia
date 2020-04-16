load('../Data/data-full/dataFull.Rdata')

write.csv(X,"../Data/data-full/dataFull.csv", row.names = FALSE)
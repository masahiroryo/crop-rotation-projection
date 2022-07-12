load(file="./output/sim_updated.RData")

dat$CType <- as.factor(as.numeric(dat$CType))

# frequency of each crop type
ggplot(dat) + 
  geom_bar(mapping = aes(x = CType))

write.csv(eco_factors, file = "C:/Users/FOL/Desktop/Fisheries-Resilience-Index/factores/eco_factors.csv")
write.csv(ins_factors, file = "C:/Users/FOL/Desktop/Fisheries-Resilience-Index/factores/ins_factors.csv")
write.csv(soc_factors, file = "C:/Users/FOL/Desktop/Fisheries-Resilience-Index/factores/soc_factors.csv")
x <- merge(eco_factors, ins_factors)
View(x)
x <- merge(eco_factors, ins_factors, by = intersect(COUNTRIES))

x <- merge(eco_factors, ins_factors, by = COUNTRIES)

x <- merge(eco_factors, ins_factors, by = "COUNTRIES", all.x=T)
y<- merge(x, soc_factors, by = "COUNTRIES", all.x=T)
 write.csv(y, file = "C:/Users/FOL/Desktop/Fisheries-Resilience-Index/factores/factors.csv")
 View(y)
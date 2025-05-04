zero_trait_data <- read.csv('data/zerovariate_data.csv')
pars_init <- read.csv('data/pars_init.csv')
zero_trait_data$data <- as.numeric(zero_trait_data$data)
metadata <- read.csv('data/DEB_species_metadata.csv')
taxon_list <- read.csv('config/taxon_list.csv')
taxonomy <- unique(read.csv('data/taxonomy.csv'))
taxonomy$species <- paste0(taxonomy$genus, '_', taxonomy$species0)

zero_trait_data <- merge(zero_trait_data, metadata, by.x = 'species', by.y = 'speciesname', all.x = TRUE)
zero_trait_data <- merge(zero_trait_data, taxonomy, by.x = 'species', by.y = 'species', all.x = TRUE)

non.temperature.traits <- zero_trait_data$parameter[substr(zero_trait_data$parameter, 1, 4) != 'temp']

TA <- subset(pars_init, parameter == 'T_A')
WWb <- subset(zero_trait_data, parameter == 'Wwb' & units == 'g')
Wdb <- subset(zero_trait_data, parameter == 'Wdb' & units == 'g')
WWp <- subset(zero_trait_data, parameter == 'Wwp' & units == 'g')
WWi <- subset(zero_trait_data, parameter == 'Wwi' & units == 'g')
Lb <- subset(zero_trait_data, parameter == 'Lb')
Lp <- subset(zero_trait_data, parameter == 'Lp')
Li <- subset(zero_trait_data, parameter == 'Li')
Ri <- subset(zero_trait_data, parameter == 'Ri')
ab <- subset(zero_trait_data, parameter == 'ab')
ap <- subset(zero_trait_data, parameter == 'ap')

# rename trait col to trait value
colnames(WWb)[5] <- 'Wwb'
colnames(Wdb)[5] <- 'Wdb'
colnames(WWp)[5] <- 'Wwp'
colnames(WWi)[5] <- 'Wwi'
colnames(Lb)[5] <- 'Lb'
colnames(Lp)[5] <- 'Lp'
colnames(Li)[5] <- 'Li'
colnames(Ri)[5] <- 'Ri'
colnames(ab)[5] <- 'ab'
colnames(ap)[5] <- 'ap'


ab.temp <- subset(zero_trait_data, parameter == 'temp.ab')
ab <- merge(ab, ab.temp[, c(1,5)], by = 'species')
ab <- merge(ab, WWb[, c(1,5)], by = 'species')
ab <- merge(ab, TA[, c(3,4)], by = 'species')
cols <- length(colnames(ab))
colnames(ab)[(cols - 2):cols] <- c('T_b', 'Ww_b', 'T_A')

ap.temp <- subset(zero_trait_data, parameter == 'temp.ap')
ap <- merge(ap, ap.temp[, c(1,5)], by = 'species')
ap <- merge(ap, WWp[, c(1,5)], by = 'species', all.x = TRUE)
ap <- merge(ap, TA[, c(3,4)], by = 'species')
cols <- length(colnames(ap))
colnames(ap)[(cols - 2):cols] <- c('T_b', 'Ww_p', 'T_A')

Ri.temp <- subset(zero_trait_data, parameter == 'temp.Ri')
Ri <- merge(Ri, Ri.temp[, c(1,5)], by = 'species')
Ri <- merge(Ri, WWi[, c(1,5)], by = 'species')
Ri <- merge(Ri, WWb[, c(1,5)], by = 'species')
Ri <- merge(Ri, TA[, c(3,4)], by = 'species')
cols <- length(colnames(Ri))
colnames(Ri)[(cols - 3):cols] <- c('T_b', 'Ww_i', 'Ww_b', 'T_A')


T_correct <- function(data, T_REF = 293.15){
  data$T_cor <- exp(data$T_A / T_REF - data$T_A / data$T_b)
  data <- subset(data, T_cor < 20)
  plot((data$T_b - 273.15), data$T_cor)
  abline(h = 1)
  abline(v = 20)
  return(data)
}
ab <- T_correct(ab) 
ap <- T_correct(ap)
Ri <- T_correct(Ri)

ab$ab_20 <- ab$ab / ab$T_cor
ap$ap_20 <- ap$ap / ap$T_cor
Ri$Ri_20 <- Ri$Ri / Ri$T_cor

plot(log10(ab$Ww_b), ab$ab_20, ylim = c(0, 365))
plot(log10(ap$Ww_p), ap$ap_20)
plot(log10(Ri$Ww_i), log10(Ri$Ri_20 * Ri$Ww_b))
lm.repro <- lm(log10(Ri$Ri_20 * Ri$Ww_b) ~ log10(Ri$Ww_i))
abline(lm.repro)

unique(Ri$class)
unique(Ri$order[Ri$class == 'Mammalia'])

Ri_squam <- subset(Ri, class == 'Arachnida')
plot(log10(Ri_squam$Ww_i), log10(Ri_squam$Ri_20 * Ri_squam$Ww_b))
lm.repro <- lm(log10(Ri_squam$Ri_20 * Ri_squam$Ww_b) ~ log10(Ri_squam$Ww_i))
abline(lm.repro)
lm.repro

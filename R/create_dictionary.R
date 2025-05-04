# first delete everything below line 4 in the traits.yml file
file <- 'config/traits.yml'
on.exit(close(file))

deb_traits <- read.csv('data/DEB_estimation_traits_zerovariate.csv')

for(i in 1:nrow(deb_traits)){

# add trait name
trait <- deb_traits$Data_label[i]
txt <- paste0("    ", trait, ":\n")
cat(txt, file = file, append = TRUE)

# add trait values
traits <- tibble::tibble(label = deb_traits$Trait_name[i], description = deb_traits$Description_full[i],
                         type = 'numeric', units = deb_traits$Units[i], allowed_values_min = deb_traits$Minimum[i],
                         allowed_values_max = deb_traits$Maximum[i])
txt <- yaml::as.yaml(traits, column.major = FALSE, indent = 6) %>% 
  gsub(": ~", ":", ., fixed = TRUE) %>% 
  gsub("^-", " ", ., perl = TRUE) %>%
  gsub("\\.inf", "Inf", .)

if (!stringr::str_sub(txt, nchar(txt)) == "\n") 
  txt <- c(txt, "\n")
cat(txt, file = file, append = TRUE)
}

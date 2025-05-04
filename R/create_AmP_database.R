library(traits.build)
library(yaml)
library(rgbif)
library(rcrossref)

replace_text_in_file <- function(file_path, find_text, replace_text, output_path = file_path) {
  
  # Read the file as a vector of lines
  text_lines <- readLines(file_path, warn = FALSE)
  
  # Replace occurrences of the find_text with replace_text
  modified_lines <- gsub(find_text, replace_text, text_lines, fixed = TRUE)
  
  # Write the modified content back to the file (or a new file)
  writeLines(modified_lines, output_path)
  
  message("File updated: ", output_path)
}

# combine all zero trait data
data.dir <- "data/csv_files"
ls.species <- gsub(pattern = "_zero_traits.csv",
                 replacement = "",
                 x = list.files(data.dir, pattern="_zero_traits.csv"))

x = list.files(data.dir, pattern="_zero_traits.csv", full=TRUE)
zero_trait_data <- do.call("rbind", lapply(x, FUN = function(files){read.csv(files)}))

#Check for duplicates here
dups<-which(duplicated(zero_trait_data))
if(length(dups)>0){
  zero_trait_data<-zero_trait_data[-dups,]
}

# these are the trait names:
unique(zero_trait_data$reference_trait_name)[order(unique(zero_trait_data$reference_trait_name))]

zero_trait_data$sex<-NA
zero_trait_data$f_level<-NA
zero_trait_data$temperature_treatment<-NA
zero_trait_data$temperature_sensitive<-0
males<-grep(x=zero_trait_data$parameter,"_m")
females<-grep(x=zero_trait_data$parameter,"_f") # but this doesn't always mean female - can mean food level
food_levels <- grep(x=zero_trait_data$label[females], 'food')
females <- females[-food_levels]
zero_trait_data$sex[males]<-"male"
zero_trait_data$sex[females]<-"female"
males<-grep(x=zero_trait_data$label,"for males")
zero_trait_data$sex[males]<-"male"
#temperatures
temp_treatments<-grep(x=zero_trait_data$parameter,"_[0-9]")
zero_trait_data$temperature_treatment[temp_treatments]<-as.numeric(str_split_fixed(pattern = "_", string = as.character(zero_trait_data$parameter[temp_treatments]),n=2)[,2])
#Others have '_T' followed by temperature 
temp_treatments<-grep(x=zero_trait_data$parameter,"_T[0-9]")
zero_trait_data$temperature_treatment[temp_treatments]<-as.numeric(str_split_fixed(pattern = "_T", string = as.character(zero_trait_data$parameter[temp_treatments]),n=2)[,2])
#Others just have the temperature directly after the trait name with no '_', but need to be careful because this is done for measurements made at a particular size too...
#These size-specific values can be eliminated by checking if the traits are temperature sensitive:
temp_values<-grep(x=zero_trait_data$parameter,"temp.")
zero_trait_data$temperature_sensitive[which(substr(zero_trait_data$parameter, 1, 1) == 'a')] <- 1
zero_trait_data$temperature_sensitive[which(substr(zero_trait_data$parameter, 1, 1) == 't')] <- 1
zero_trait_data$temperature_sensitive[which(substr(zero_trait_data$parameter, 1, 1) == 'd')] <- 1
zero_trait_data$temperature_sensitive[which(substr(zero_trait_data$parameter, 1, 2) == 'Ri')] <- 1
zero_trait_data$temperature_sensitive[which(substr(zero_trait_data$parameter, 1, 2) == 'rB')] <- 1
zero_trait_data$temperature_sensitive[temp_values] <- 0 # these are the temp. values
# now remove temp treatments from those that are not temperature sensitive
zero_trait_data$temperature_treatment[zero_trait_data$temperature_sensitive == 0]<-NA

#Resource levels/f-levels
resource_treatments<-grep(x=zero_trait_data$parameter,"_f[0-9]")
zero_trait_data$f_level[resource_treatments]<-as.numeric(str_split_fixed(pattern = "_f", string = as.character(zero_trait_data$parameter[resource_treatments]),n=2)[,2])
#Get rid of empty rows (for temp but no data)
zero_trait_data<-zero_trait_data[!is.na(zero_trait_data$parameter),]
zero_trait_data <- subset(zero_trait_data, !(is.na(standard_trait_name)))
write.csv(zero_trait_data, file = 'data/zerovariate_data.csv')

x <- list.files(data.dir, pattern = "_pars_init_traits.csv", full = TRUE)
pars_init_data <- do.call("rbind", lapply(x, FUN = function(files){read.csv(files)}))
write.csv(pars_init_data, 'data/pars_init.csv')

taxonomy <- unique(read.csv('data/taxonomy.csv'))
taxonomy$species <- paste0(taxonomy$genus, '_', taxonomy$species0)
taxon_list <- read.csv('config/taxon_list_orig.csv')
taxon_list$climate <- NA
taxon_list$ecozone <- NA
taxon_list$habitat <- NA
taxon_list$embryo <- NA
taxon_list$migrate <- NA
taxon_list$food <- NA
taxon_list$gender <- NA
taxon_list$reprod <- NA
taxon_list_new <- taxon_list[0, ]

for(i in 1:nrow(taxonomy)){
  taxon_row <- nrow(taxon_list_new) + 1
  classification <- taxonomy[which(paste(taxonomy$genus, taxonomy$species0) == species), ]
  #gbif <- rgbif::name_lookup(species, rank = 'species', status = 'accepted', limit = 1)
  taxon_list_new[taxon_row, 1] <- species
  taxon_list_new[taxon_row, 2] <- species
  taxon_list_new[taxon_row, 3] <- 'species'
  taxon_list_new[taxon_row, 4] <- 'accepted'
  taxon_list_new[taxon_row, 5] <- 'gbif'
  taxon_list_new[taxon_row, 7] <- classification$genus
  taxon_list_new[taxon_row, 8] <- classification$family
  taxon_list_new[taxon_row, 9] <- species
  #taxon_list_new[taxon_row, 14] <- gbif$data$key
  taxon_list_new[taxon_row, 20] <- classification$climate
  taxon_list_new[taxon_row, 21] <- classification$ecozone
  taxon_list_new[taxon_row, 22] <- classification$habitat
  taxon_list_new[taxon_row, 23] <- classification$embryo
  taxon_list_new[taxon_row, 24] <- classification$migrate
  taxon_list_new[taxon_row, 25] <- classification$food
  taxon_list_new[taxon_row, 27] <- classification$gender
  taxon_list_new[taxon_row, 28] <- classification$reprod

}
write.csv(taxon_list_new, file = 'config/taxon_list.csv')


data <- read.csv('data/zerovariate_data.csv')
#ecocodes <- read.csv('data/ecocodes.csv')
metadata <- read.csv('data/DEB_species_metadata.csv')
data1 <- merge(data, metadata, by.x = 'species', by.y = 'speciesname', all.x = TRUE)


traits <- unique(data1$reference_trait_name)
check=subset(data1, is.na(reference_trait_name))
check2=subset(data1, !is.na(reference_trait_name))
unique(check$parameter)
unique(check2$reference_trait_name)
traits_notemp <- sort(traits[-grep('temp.', traits)])
'Ls = length at start of acceleration'
'WC is carbon weight'
'JCi not in list of traits'
'Eh is energy content at hatch'
'pL not in list of traits'
'xi_WE not in list of traits'
'Lr not in list of traits but LR is (length at repro)'
'JO not in list of traits'


#traits <- 'am'
fishbase_count <- 1
sealifebase_count <- 1
Wiki_count <- 1
ADW_count <- 1
for(trait in traits){
  subtrait <- subset(data1, parameter == trait)
  for(i in 40:nrow(subtrait)){
    # get data
    subdata <- subtrait[i, ]
    # create folder for data
    dataID <- paste0(trait, '_', subdata$bibkey)
    if(!subdata$bibkey == 'guess'){
      if(subdata$bibkey == 'fishbase'){
        dataID <- paste0(dataID, '_', fishbase_count)
        fishbase_count <- fishbase_count + 1
      }
      if(subdata$bibkey == 'sealifebase'){
        dataID <- paste0(dataID, '_', sealifebase_count)
        sealifebase_count <- sealifebase_count + 1
      }
      if(subdata$bibkey == 'ADW'){
        dataID <- paste0(dataID, '_', ADW_count)
        ADW_count <- ADW_count + 1
      }
      if(subdata$bibkey == 'Wiki'){
        dataID <- paste0(dataID, '_', Wiki_count)
        Wiki_count <- Wiki_count + 1
      } 
      dataPath <- paste0('data/', dataID)
      dir.create(dataPath)
      # save the data
      write.csv(subdata, file = paste0(dataPath, '/data.csv'))
      # create metadata.yml file
      metadata_create_template(dataset_id = dataID,
                               path = dataPath,
                               skip_manual = FALSE,
                               user_responses = list(data_is_long_format = TRUE,
                                                     taxon_name = 'species',
                                                     trait_name = 'parameter',
                                                     value = 'data',
                                                     #description = 'verbatimLabel',
                                                     #notes = 'measurementRemarks',
                                                     location_name = NA,
                                                     individual_id = NA,
                                                     collection_date = NA)
      )
      # add trait to metadata.yml
      # check if new species for the database
      species <- gsub("_", " ", subdata$species)

      metadata_add_traits(dataset_id = dataID,
                          user_responses = list(var_in = trait,
                                                unit_in = subdata$units, 
                                                trait_name = trait, 
                                                entity_type = "unknown", 
                                                value_type = "unknown", 
                                                basis_of_value = "measurement", 
                                                replicates = "unknown", 
                                                methods = "unknown")
      )
      replace_text_in_file(paste0(dataPath, "/metadata.yml"), "unit_in: unknown", paste0("unit_in: ", subdata$units))
      replace_text_in_file(paste0(dataPath, "/metadata.yml"), "trait_name: unknown", paste0("trait_name: ", trait))
      replace_text_in_file(paste0(dataPath, "/metadata.yml"), "dataset_curators: unknown", paste0("dataset_curators: ", paste(subdata$entry_author, subdata$entry_curator, sep = '; ')))
      if(is.na(subdata$doi)){
        if(!(subdata$bibkey == 'fishbase' | 
             subdata$bibkey == 'ADW' | 
             subdata$bibkey == 'sealifebase' | 
             subdata$bibkey == 'Wiki')){
          res <- cr_works(query_title = subdata$title, filter = c(from_pub_date = subdata$year, to_pub_date = subdata$year))
        }
      }else{
        metadata_add_source_doi(
          dataset_id = dataID, 
          doi = subdata$doi
        )
      }
    }
  }
}
# adding taxon in metadata (if incorrect)
#metadata_add_taxonomic_change(dataset_id = "Kearney_2024_1", find = "Ctenophorus isolepisNT", replace = "Ctenophorus isolepis", 
#                              taxonomic_resolution = "Species", reason = "removing concatenation")

build_setup_pipeline(method = "base", database_name = "traits.build_database")

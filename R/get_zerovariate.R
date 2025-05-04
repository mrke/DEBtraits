library(stringr)
library(XML)
library(rcrossref)

source('R/get_entry_zip.R')
source('R/extract_zerovariate.R')
source('R/extract_metadata.R')
source('R/extract_weights.R')

# get list of zip files available in the archive

# url <- "http://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries/"
# AmP_folders <- XML::getHTMLLinks(
#   url,
#   xpQuery = "//a/@href['/' = substring(., string-length(.), 1)]"
# )
# AmP_folders <- sort(AmP_folders)[-1]

reset <- TRUE

if(reset){
  file.remove('deb_csv_error_messages.txt')
  file.remove('data/DEB_species_metadata.csv')
  file.remove('data/taxonomy.csv')
  file.remove(list.files('data/csv_files', full.names = TRUE))
  unlink('data/mydata_files', recursive = TRUE)
}

url <- "http://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries_zip/"
zips <- XML::getHTMLLinks(
  url, 
  xpQuery = "//a/@href['.zip'=substring(., string-length(.) - 3)]"
)
zips<-sort(zips)

DEB_traits <- read.csv('data/DEB_estimation_traits_zerovariate.csv')

generate_DEB_csvs <- function(f = f, 
                              zips = zips){
  
  out<-tryCatch( #Catch errors in formatting/extract code
    {
      filename <- zips[f]
      speciesname <- strsplit(filename, split = "_")
      speciesname <- paste(speciesname[[1]][1:((length(speciesname[[1]]) - 1))], collapse="_", sep = '_')
      
      # get my_data and pars_init files
      trait_files <- get_entry(file = filename, check.download = TRUE)
      zero_trait_list <- DEB_traits$Data_label[DEB_traits$Data_type == "Zerovariate"]
      
      #Get rid of separate entries for multiple T or multiple f
      excl <- grep("several", DEB_traits$Description[DEB_traits$Data_type == "Zerovariate"])
      zero_trait_list<-zero_trait_list[-c(excl)]
      zero_trait_data <- extract_zerovariate(zero_trait_list = zero_trait_list,
                                             trait_files = trait_files,
                                             speciesname = speciesname, 
                                             write_output = FALSE)
      
      
      short_refs <- unique(levels(as.factor(zero_trait_data$bibkey)))
      short_refs <- unique(unlist(strsplit(x = short_refs,"\\{|\\}|\\,")))
      short_refs <- str_subset(short_refs, ".+")
      short_refs <- str_subset(short_refs, "guess",negate = TRUE)
      short_refs<- trimws(short_refs)
      
      metadata.ls <- extract_metadata(speciesname = speciesname, 
                                    filename = filename,
                                    short_refs = short_refs,
                                    trait_files = trait_files,
                                    write_output = TRUE)
      
      
      
      data_add_weights <- extract_weights(speciesname = speciesname, 
                                        trait_files = trait_files,
                                        zero_trait_data = zero_trait_data,
                                        univariate_trait_data = NULL)
      
      zero_trait_data <- data_add_weights$zero_trait_data
      lookup <- as.data.frame(metadata.ls$ref_table, stringsAsFactors=FALSE)
      merged_data <- merge(zero_trait_data, lookup, by.x = "bibkey", by.y = "short_ref", all.x = TRUE)
      
      ##############################################
      ## Step 6: Write output   ####################
      ##############################################
      
      if(isTRUE(write_csv_output)){ #if option turned on, write output
        
        write.csv(merged_data, file = paste0("data/csv_files/",speciesname,"_zero_traits.csv"), row.names = FALSE)

      }
      
      ##########################################################
      # Step 7: Clean-up (remove downloaded files)  ############
      ##########################################################
      
      unlink(paste0('data/mydata_files/', speciesname), recursive = TRUE) #get rid of folder with downloaded mydata & csv files
      
      
    },
    
    error=function(cond) {
      message(paste("Failed:", speciesname))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      lines_out<-(paste0("Error in f=", f, " ", speciesname, ":", as.character(cond)))
      cat(lines_out,
          file="deb_csv_error_messages.txt",
          append=TRUE)
      
      return(NA)
    }
    # },
    # 
    # finally={
    #   message(paste("Processed species:", speciesname))
    # }
  )    
  return(out)
  
} #end of processing for species f
write_csv_output <- TRUE
f <- seq(1,length(zips))
#f <- seq(1,50)
y <- lapply(f, generate_DEB_csvs, zips=zips)
#generate_DEB_csvs(f=1, zips=zips)

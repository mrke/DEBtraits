# Function to download .zip archive folders, and extract and read in mydata_X.m files
get_entry <- function(filename = "myfile", check.download = TRUE){
  url <- 'http://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries/'
  speciesname <- strsplit(filename, split = "_")
  speciesname <- paste(speciesname[[1]][1:((length(speciesname[[1]]) - 1))], collapse="_", sep = '_')
  mydata_filename <- paste0('mydata_', speciesname, '.m')
  pars_init_filename <- paste0('pars_init_', speciesname, '.m')
  mydata_file <- paste0('data/mydata_files/', speciesname, '/', mydata_filename)
  pars_init_file <- paste0('data/mydata_files/', speciesname, '/', pars_init_filename)
  downloaded <- FALSE
  if(check.download){
    if(dir.exists(paste0("data/mydata_files/",speciesname))){
      mydata_check <- list.files(path = paste0("Data/mydata_files/",speciesname,"/"), pattern = "mydata", full.names = TRUE)
      if(length(mydata_check) == 1){
        downloaded <- TRUE
      }
    }
  }
  if(!downloaded){
    temp <- tempfile()
    
    download.file(paste0("http://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries_zip/",filename),temp)
    
    mydata_file<-utils::unzip(temp,
                              files = paste0(speciesname,"/mydata_",speciesname,".m"),
                              exdir = "data/mydata_files/")
    
    pars_init_file<-utils::unzip(temp,
                                 files = paste0(speciesname,"/pars_init_",speciesname,".m"),
                                 exdir = "data/mydata_files/")
    
  }   
  mydata <- readLines(con = mydata_file, skipNul = TRUE)
  pars_init <- readLines(con = pars_init_file, skipNul = TRUE)

  data_out <- list(mydata = mydata, pars_init = pars_init)
  
  return(data_out)
}
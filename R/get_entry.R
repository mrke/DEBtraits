# Function to download .zip archive folders, and extract and read in mydata_X.m files
get_entry <- function(species = "myfile", check.download = TRUE){
  url <- 'http://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries/'
  speciesname <- gsub('/', '', species)
  mydata_filename <- paste0('mydata_', speciesname, '.m')
  pars_init_filename <- paste0('pars_init_', speciesname, '.m')
  mydata_file <- paste0('data/mydata_files/', species, mydata_filename)
  pars_init_file <- paste0('data/mydata_files/', species, pars_init_filename)
  downloaded <- FALSE
  if(check.download){
    if(dir.exists(paste0("data/mydata_files/",gsub('/', '', species),"/"))){
      mydata_check <- list.files(path = paste0("Data/mydata_files/",speciesname,"/"), pattern = "mydata", full.names = TRUE)
      if(length(mydata_check) == 1){
        downloaded <- TRUE
      }
    }
  }
  if(!downloaded){
    dir.create(paste0('data/mydata_files/', species))
    download.file(paste0(url, species, mydata_filename), mydata_file)
    download.file(paste0(url, species, pars_init_filename), pars_init_file)
  }   
  mydata <- readLines(con = mydata_file, skipNul = TRUE)
  pars_init <- readLines(con = pars_init_file, skipNul = TRUE)

  data_out <- list(mydata = mydata, pars_init = pars_init)
  
  return(data_out)
}
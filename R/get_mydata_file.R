# Function to download .zip archive folders, and extract and read in mydata_X.m files
get_mydata_file <- function(filename = "myfile", check.download = TRUE){
  
  #speciesname<-sub("/$", "", foldername)
  
  #speciesname<-str_remove_all(filename, pattern = "[:digit:]")
  speciesname<-strsplit(filename, split = "_")
  speciesname<-paste(speciesname[[1]][1:((length(speciesname[[1]]) - 1))], collapse="_", sep = '_')
  
  #speciesname<-str_remove_all(speciesname,pattern = "_.zip")
  downloaded <- FALSE
  if(check.download){
    if(dir.exists(paste0("Data/mydata_files/",speciesname,"/"))){
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
                              files = paste0(speciesname,"/mydata_",speciesname,".m"))
    
    pars_init_file<-utils::unzip(temp,
                                 files = paste0(speciesname,"/pars_init_",speciesname,".m"))
    
    ##Also download and read any uni- or bi-variate data files
    list_files<-utils::unzip(zipfile = temp, list = TRUE)
    
    files_csv<-list_files[str_detect(list_files[,1],"csv"),1]
    
    
    csv_data <- list()
    
    
    for (i in seq_along(files_csv)) {
      
      csv_data[[i]] <- read.csv(unz(temp, files_csv[i]), header = FALSE,
                                sep = ",") 
      
    }
    
    names(csv_data) <- str_match(files_csv,"\\/(.*?).csv")[,2]
    
    unlink(temp)
    
    main_datafile_lines<-readLines(con = mydata_file,
                                   skipNul = TRUE)
    pars_datafile_lines<-readLines(con = pars_init_file,
                                   skipNul = TRUE)
    
  }else{
    main_datafile_lines<-readLines(con = paste0("Data/mydata_files/",speciesname,"/mydata_",speciesname,".m"),
                                   skipNul = TRUE)
    pars_datafile_lines<-readLines(con = paste0("Data/mydata_files/",speciesname,"/pars_init_",speciesname,".m"),
                                   skipNul = TRUE) 
    csv_data <- list()
    
    files_csv <- list.files(path = paste0("Data/mydata_files/",speciesname,"/"), pattern = "\\.csv$", full.names = TRUE)
    
    for (i in seq_along(files_csv)) {
      
      csv_data[[i]] <- read.csv(unz(temp, files_csv[i]), header = FALSE,
                                sep = ",") 
      
    }
  }
  
  data_out<-list(main_datafile_lines=main_datafile_lines,
                 pars_datafile_lines = pars_datafile_lines,
                 csv_data = csv_data)
  
  return(data_out)
}
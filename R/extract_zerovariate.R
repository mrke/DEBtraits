#Function to assign zero-trait values & compile into trait data file
extract_zerovariate <- function(speciesname = speciesname,
                                zero_trait_list = zero_trait_list,
                                trait_files = data_all,
                                write_output = TRUE){
  extract_val <- function(string2find, trait_file_txt){
    stringr::str_trim(strsplit(trait_file_txt[grepl(string2find,trait_file_txt)],";|=")[[1]][2])
  }
  #unpack data 
  
  #mydata file
  trait_file_txt<-unlist(trait_files$mydata)
  
  trait_file_txt<-str_remove_all(trait_file_txt, "'")
  
  #pars_init
  pars_init<-unlist(trait_files$pars_init)
  pars_init<-str_remove_all(pars_init, "'")
  
  #Set up empty vector for species traits list & empty dataframe for trait names
  sp_zero_trait_list<-NULL
  sp_zero_trait_lookup<-data.frame()
  
  sp_fun_response_list<-NULL
  sp_fun_response_lookup<-data.frame()
  
  ##first get thermal response and f data from pars init 
  # save these to sp_fun_response_list
  to_find<-c("par.T_A","par.f","par.T_AH","par.T_AL", "par.T_AP", "par.T_H")
  
  for(par in to_find){
    
    par_listed<-par #save reference name
    
    l<-pars_init[grep(pattern = par,
                      x=pars_init)] # find row
    
    ##Check if multiple entries 
    if(length(l)>0){ #if there is information for this trait
      
      ##First check if trait values are specified for sex or life stage 
      par_descript<-as.character(str_match(l, paste0(par,"(.*?)\\s*\\=\\s*"))[,2])
      
      #issues when equations refer to trait_txt, not sure why code above doens't excluded these? just add this check for now
      
      par_descript<-par_descript[!grepl(";",par_descript) & !is.na(par_descript)]
      
      if(length(par_descript)==0){
        par_descript<-""
      }
      
      # Repeat the process below for each set of data (sex/life-stage/temp/food level) - note that in some cases 'base' data (e.g. adults) have no descriptor, with eggs as _egg
      
      par_ref<-par #save reference name
      
      for(d in par_descript){ #get list of suffixes, if nothing will get "" 
        
        par<-paste0(par_ref,d)
        
        # print(trait_txt) for checking
        
        #Re-extract the relevant lines of code - just for this specific set of data
        
        l<-pars_init[grep(pattern = paste0(par,"\\s*\\="),
                          x=pars_init)]
        
        #in some cases parameter was just mentioned, not defined
        if(length(l)==0){
          
          assign(trait_txt, NA)
          
        }else{
          
          val<-str_match(l, paste0(par,"\\s+=\\s*(.*?)\\s*[;]"))[,2]
          
          # added this in for cases where Arrhenius pars are specified with C2K
          if(grepl(pattern = "C2K", x=val)){ #need to convert to Kelvin
            val_c<-str_match(val, "(?<=\\().+?(?=\\))")
            val<-as.numeric(val_c) + 273.15
          }else{
            val<-val #otherwise don't change to numeric
          }
          
          
          #Convert data values to numeric
          val <- eval(parse(text=val)) # deals with situations when value = function
          
          assign(par, val)
          
          #also save label for par (all should have this, useful for checking when have multiple values for different sexes/measurements)
          
          par_label<-gsub(pattern = "par",
                          replacement = "label",
                          x = par)
          l<-pars_init[grep(pattern = paste0(par_label,"\\s*\\="),
                            x=pars_init)]
          val<-str_match(l, paste0(par_label,"\\s+=\\s*(.*?)\\s*[;]"))[,2]
          
          assign(par_label, val)
          
          #Also assign units - all K except f, these are '-'
          par_units<-gsub(pattern = "par",
                          replacement = "units",
                          x = par)
          
          if(par_listed =="par.f"){
            assign(par_units,"-")
            
          }else{
            assign(par_units, "K")
            
          }
          
          #save additional trait list
          sp_fun_response_list<-c(sp_fun_response_list,par)
          trait_names<-as.data.frame(cbind(par_listed, par_ref,par)) #Keep track of trait names
          sp_fun_response_lookup<-rbind(sp_fun_response_lookup,trait_names)
        }
        
      } #end loop for sex/lifestage/temp/food level
      
    }else{
      
      assign(par, NA)
    } #otherwise, assign NA
  }
  
  ##Get this into a useful format to write out
  
  #Set up dataframe to store output
  pars_init_table<-setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("species","parameter","data","units","label"))
  
  pars_init_table_row<-setNames(data.frame(matrix(ncol = 5, nrow = 1)), c("species", "parameter","data","units","label"))
  
  sp_fun_response_list<-sp_fun_response_list[sp_fun_response_list!="par.free"]
  
  for(par in sp_fun_response_list){
    
    if(is.na(mget(paste0(par)))){
      stop(paste0("Error - data for ",par," missing"))
    }
    
    if(!is.na(mget(paste0(par),ifnotfound = NA))){ #include only if there is data, not all measurements made for all species
      trait_txt<-gsub(pattern="par.",
                      replacement = "",
                      par)
      
      trait_info<-paste0(c("par.","units.","label."),trait_txt)
      
      trait_vals<-c(speciesname,trait_txt,mget(trait_info))
      
      pars_init_table_row[1,1:5]<-trait_vals[1:5]
      
      pars_init_table<-rbind( pars_init_table, pars_init_table_row)
    }
    
  }
  
  ##Add info on alternative trait names 
  colnames(sp_fun_response_lookup)<-c("standard_trait_name", "reference_trait_name","parameter")
  
  sp_fun_response_lookup<-sp_fun_response_lookup[sp_fun_response_lookup$parameter!="par.free",]
  
  sp_fun_response_lookup<-sapply(sp_fun_response_lookup, function(x)
    gsub(paste("par.", collapse = '|'), '', x))
  
  pars_init_table<-merge(pars_init_table,sp_fun_response_lookup, by = "parameter")
  
  
  ######Zero trait data from mydata file #######################
  
  #extract list of zero trait data included in the file from metadata statement 
  l_st<-grep(pattern = "metaData.data_0",
             x=trait_file_txt)
  
  l_ends<-grep(pattern = "\\}\\;",
               x=trait_file_txt)
  l_end<-l_ends[l_ends>=l_st][1]
  
  meta_0<-trimws(str_split(str_extract(trait_file_txt[l_st:l_end],
                                       "(?<=\\{)(.*)(?=\\})"),";", simplify=TRUE))
  
  meta_0<-trimws(str_split(meta_0,",", simplify=TRUE)) #sometimes separated by "," not ";"
  meta_0<-str_remove_all(meta_0, "'") # added by MK 1/2/25
  meta_0<-meta_0[meta_0 != ""] # added by MK 3/2/25
  
  # first expand traits x trait types
  
  trait_types<-paste0(c("data","units","label","bibkey","comment",
                        "temp","units.temp","label.temp"),".")
  
  zero_traits<-do.call(paste0, expand.grid(trait_types,
                                           meta_0))
  
  #zero_traits<-do.call(paste0, expand.grid(trait_types,
  #                                         zero_trait_list))
  
  
  #Assign values to zero-variate traits
  
  for(trait_txt in zero_traits){
    
    trait_txt_listed<-trait_txt #save reference name
    
    # print(paste0("ref = ",trait_txt_ref)) for checking
    
    l_names<-length(strsplit(trait_txt,"\\.")[[1]])
    
    trait_type<-paste(strsplit(trait_txt,"\\.")[[1]][1:(l_names-1)],collapse =".")
    
    trait_txt<-strsplit(trait_txt,"\\.")[[1]][l_names]
    
    ##Get list of all terms to search for 
    #trait_txt_alt<-trimws(unlist(str_split(paste0(trait_txt,",",trimws(DEB_traits$Data_label_synonyms[DEB_traits$Data_label==trait_txt]), DEB_traits$Data_label_synonyms[grep(paste0("^",trait_txt,"$"), DEB_traits$Data_label_synonyms)]), ",")))
    trait_txt_alt <- unique(trimws(unlist(str_split(
      paste(
        trait_txt, # Always include the original trait_txt
        DEB_traits$Data_label_synonyms[DEB_traits$Data_label == trait_txt], # Synonyms for this official label
        DEB_traits$Data_label[grepl(paste0("\\b", trait_txt, "\\b"), DEB_traits$Data_label_synonyms)], # Official label(s) where trait_txt appears as a synonym
        sep = ","
      ),
      ","
    ))))
    trait_txt_alt<-trait_txt_alt[trait_txt_alt != ""]
    
    trait_txt_alt<-unique(paste0(trait_type,".",trait_txt_alt))
    
    l<-trait_file_txt[grep(pattern = paste(trait_txt_alt, collapse = "|"),
                           x=trait_file_txt)] # header row, use original units
    l <- l[!grepl("^%", l)]
    ##Identify which is the trait term used 
    trait_txt<- trait_txt_alt[sapply(trait_txt_alt, function(x) grepl(x, l[1]))][1] #Note that if use label with '_e' instead of '_egg', the extra text will be added by the code below
    
    # remove 'data' and just search for all cases of '.trait'    
    trait_txt2 <- gsub("data", replacement = "", trait_txt)
    #l<-trait_file_txt[grep(pattern = paste0("\\", trait_txt2),
    #                       x=trait_file_txt)]
    # check for for use of ab in ap terms and delete
    if(!is.na(trait_txt)){
      if(is.character(trait_txt)){
        if(trait_txt == 'data.ab'){
          if(max(grep("data.ap", l)) > 0){
            l<-l[-grep("data.ap", l)]
          }
        }
      }
    }
    
    if(length(l)>0){ #if there is information for this trait
      
      ##First check if trait values are specified for sex or life stage 
      trait_descript<-as.character(str_match(l, paste0(trait_txt,"(.*?)\\s*\\=\\s*"))[,2])
      
      #issues when equations refer to trait_txt, not sure why code above doens't excluded these? just add this check for now
      
      trait_descript<-trait_descript[!grepl(";",trait_descript) & !is.na(trait_descript)]
      
      if(length(trait_descript)==0){
        trait_descript<-""
      }
      
      # Repeat the process below for each set of data (sex/life-stage/temp/food level) - note that in some cases 'base' data (e.g. adults) have no descriptor, with eggs as _egg
      
      trait_txt_ref<-trait_txt #save reference name
      if(sub(".*\\.", "", trait_txt) == 'L'){ # hack to get rid of LW data from zerovariate data
        trait_descript <- trait_descript[trait_descript != "W"]
      }
      for(d in trait_descript){ #get list of suffixes, if nothing will get "" 
        
        trait_txt<-paste0(trait_txt_ref,d)
        
        # print(trait_txt) for checking
        
        #Re-extract the relevant lines of code - just for this specific set of data
        
        # remove 'data' and just search for all cases of '.trait'    
        trait_txt2 <- gsub("data", replacement = "", trait_txt)
        l<-trait_file_txt[grep(pattern = paste0("\\", trait_txt2,"\\s*\\="),
                               x=trait_file_txt)]
        # check for for use of ab in ap terms and delete
        # if(exists(trait_txt)){
        #   if(is.character(trait_txt)){
        #   if(trait_txt == 'data.ab'){
        #     if(max(grep("data.ap", l)) > 0){
        #       l<-l[-grep("data.ap", l)]
        #     }
        #   }
        #   }
        # }
        
        l<-trait_file_txt[grep(pattern = paste0(trait_txt,"\\s*\\="),
                               x=trait_file_txt)]
        
        #in some cases parameter was just mentioned, not defined
        if(length(l)==0){
          
          assign(trait_txt, NA)
          
        }else{
          
          val<-str_match(l, paste0(trait_txt,"\\s+=\\s*(.*?)\\s*[;]"))[,2]
          
          #data values - convert to numeric
          if(grepl(pattern = "data", x= trait_txt)){ #Convert data values to numeric
            val <- eval(parse(text=val)) # deals with situations when value = function
            ##Issues = when value is defined as 3.g. 25 x 365, or as another paramter x a number
          }
          
          #temperature values - convert to numeric & into K if needed
          to_match<-c("units","label","bibkey","comment")
          if(grepl(pattern = "temp", x= trait_txt) & all(grepl(paste(to_match,collapse="|"), trait_txt)==FALSE)){ #Convert temp values to numeric
            if(grepl(pattern = "C2K", x=val)){ #need to convert to Kelvin
              val_c<-str_match(val, "(?<=\\().+?(?=\\))")
              val<-as.numeric(val_c) + 273.15
            }else{
              val<-val #otherwise don't change to numeric
            }
          }
          
          assign(trait_txt, val)
          
          #save species trait list - unless comment as these often include typos and causing errors later on
          if(!grepl(pattern = "comment", x = trait_txt)){
            sp_zero_trait_list<-c(sp_zero_trait_list,trait_txt)
            trait_names<-as.data.frame(cbind(trait_txt_listed, trait_txt_ref,trait_txt)) #Keep track of trait names
            sp_zero_trait_lookup<-rbind(sp_zero_trait_lookup,trait_names)
          }
        }
        
      } #end loop for sex/lifestage/temp/food level
      
    }else{
      
      assign(trait_txt, NA)
    } #otherwise, assign NA
  }
  
  #Add reference temp data from metadata section 
  val<-extract_val("metaData.T_typical", trait_file_txt)
  
  if(grepl(pattern = "C2K", x=val)){ #need to convert to Kelvin
    
    val_c<-str_match(val, "(?<=\\().+?(?=\\))")
    
    val<-as.numeric(val_c) + 273.15
    
  }else{
    
    val<-val #otherwise don't change to numeric
    
  }
  
  #parameters that ref temp should apply 
  
  refT_0<-trait_file_txt[grepl("metaData.data_0",trait_file_txt)]
  
  refT_0<-unlist(strsplit(str_extract(refT_0,"(?<=\\{).+?(?=\\})"),"; "))
  refT_0<-str_remove_all(refT_0, "'") # added by MK 1/2/25
  
  for(s in refT_0) {
    s_temp <- paste0("temp.", s)
    assign(s_temp, val)
    s_units <- paste0("units.temp.", s)
    assign(s_units, "K")
    s_label <- paste0("label.temp.", s)
    assign(s_label, "temperature")
  }
  
  ##Could add a comment to this row to explain what sort of reference temp is used?
  
  # write output - wide format. 
  # columns = "data","units","label","bibkey","comment"
  
  #Set up dataframe to store output
  mydata_table<-setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("species","parameter","data","units","label","bibkey","comment"))
  
  mydata_table_row<-setNames(data.frame(matrix(ncol = 7, nrow = 1)), c("species", "parameter","data","units","label","bibkey","comment"))
  
  ##Refine trait_txt so loop through names of 
  sp_zero_trait_list<-unique(str_remove_all(sp_zero_trait_list, paste(trait_types, collapse = "|")))
  
  ##Add check to deal with missing comments (frequent issue) or temp data
  for(trait_txt in sp_zero_trait_list){
    if(is.na(mget(paste0("comment.",trait_txt),ifnotfound=NA))){
      assign(paste0("comment.",trait_txt),NA)   
    }
    if(is.na(mget(paste0("temp.",trait_txt),ifnotfound=NA))){
      assign(paste0("temp.",trait_txt),NA)   
    } 
    if(is.na(mget(paste0("units.temp.",trait_txt),ifnotfound=NA))){
      assign(paste0("units.temp.",trait_txt),NA)   
    } 
    if(is.na(mget(paste0("label.temp.",trait_txt),ifnotfound=NA))){
      assign(paste0("label.temp.",trait_txt),NA)   
    } 
  }
  
  
  for(trait_txt in sp_zero_trait_list){
    
    if(is.na(mget(paste0("data.",trait_txt)))){
      stop(paste0("Error - data for data.",trait_txt," missing"))
    }
    if(!is.na(mget(paste0("data.",trait_txt),ifnotfound = NA))){ #include only if there is data, not all measurements made for all species
      
      trait_info<-paste0(c("data.","units.","label.","bibkey.","comment."),trait_txt)
      
      #first get the parameter data, then record the temp data separately 
      
      trait_vals<-c(speciesname,trait_txt,mget(trait_info))
      
      mydata_table_row[1,1:7]<-trait_vals[1:7]
      
      mydata_table<-rbind(mydata_table,mydata_table_row)
    }
    
    #add temperature data in next row, add bib & comment info from trait data
    
    if(!is.na(get(paste0("data.",trait_txt)))){ #include only if there is data
      
      trait_info<-c(paste0(c("","units.","label."),"temp.",trait_txt),
                    paste0(c("bibkey.","comment."),trait_txt))
      
      trait_vals<-c(speciesname,paste0("temp.",trait_txt),mget(trait_info))
      
      mydata_table_row[1,1:7]<-c(trait_vals[1:7])
      
      mydata_table<-rbind(mydata_table,mydata_table_row)
    }
  }
  
  ##Add info on alternative trait names 
  colnames(sp_zero_trait_lookup)<-c("standard_trait_name","reference_trait_name","parameter")
  
  sp_zero_trait_lookup<-sapply(sp_zero_trait_lookup, function(x) 
    gsub(paste(trait_types[1:5], collapse = '|'), '', x))
  
  sp_zero_trait_lookup<-sp_zero_trait_lookup[!duplicated(sp_zero_trait_lookup),]
  
  mydata_table<-merge(mydata_table,sp_zero_trait_lookup, by = "parameter", all.x = TRUE)
  
  if(isTRUE(write_output)){ #if save output = TRUE
    
    write.csv(mydata_table,file = paste0("data/csv_files/",speciesname,"_zerovariate_traits.csv"), row.names = FALSE)
    
  }
  
  write.csv(pars_init_table, file= paste0("data/csv_files/",speciesname,"_pars_init_traits.csv"), row.names = FALSE)
  
  return(mydata_table = mydata_table)
  
}
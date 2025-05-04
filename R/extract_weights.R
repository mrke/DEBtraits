extract_weights <- function(speciesname = speciesname, 
                          trait_files = data_all,
                          zero_trait_data = zero_trait_data,
                          univariate_trait_data = univariate_trait_data){
  
  #unpack data 
  
  #mydata file
  trait_file_txt<-unlist(trait_files[1])
  
  trait_file_txt<-str_remove_all(trait_file_txt, "'")
  
  #get relevant text
  l_st<-grep("%% set weights for all real data",trait_file_txt, ignore.case = TRUE)
  l_end<-grep("%% set pseudodata and respective weights",trait_file_txt, ignore.case = TRUE)-1
  l_end<-l_end[l_end>l_st][1]
  weights_text<-trait_file_txt[l_st:l_end]
  
  #Set base value of weights, use this if not defined in the mydata file
  zero_trait_data$weights<-1
  univariate_trait_data$weights<-1
  
  #For univariate data these sum to 1, so need to divide by # data points
  for(p in unique(univariate_trait_data$parameter)){
    n<-length(univariate_trait_data$parameter[univariate_trait_data$parameter==p])
    univariate_trait_data$weights[univariate_trait_data$parameter==p]<-1/n
  }
  
  
  weights_text<-gsub(pattern = "%% set weights for all real data",
                     replacement = "",
                     x = weights_text)
  
  if(length(grep("weights", weights_text))!=0){ #Run only if weights defined 
    
    #Find zero trait data weights 
    for(p in zero_trait_data$parameter){
      
      trait_weight_txt<-weights_text[grepl(pattern = paste0("weights.",p),
                                           x = weights_text)]
      
      if(length(trait_weight_txt)!=0){ # if found, extract the weight & save
        
        val<-str_match(trait_weight_txt, paste0(paste0("weights.",p),"\\s+=\\s*(.*?)\\s*\\*"))[,2]
        
        zero_trait_data$weights[zero_trait_data$parameter == p]<-val
        
      }
    } # End of loop through zero trait values
    
    
    #Find univariate trait data weights 
    for(p in unique(univariate_trait_data$parameter)){
      
      trait_weight_txt<-weights_text[grepl(pattern = paste0("weights.",p),
                                           x = weights_text)]
      
      if(length(trait_weight_txt)!=0){ # if found, extract the weight & save
        
        val<-as.numeric(str_match(trait_weight_txt, paste0(paste0("weights.",p),"\\s+=\\s*(.*?)\\s*\\*"))[,2])
        
        univariate_trait_data$weights[univariate_trait_data$parameter == p]<-univariate_trait_data$weights[univariate_trait_data$parameter == p]*val
        
      }
    } # End of loop through univariate trait values
    
  } # End of code to extract weight data
  
  out<-list(zero_trait_data=zero_trait_data,
            univariate_trait_data = univariate_trait_data)
  
  return(out)
  
}
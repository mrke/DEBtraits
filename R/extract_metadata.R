extract_metadata <- function(speciesname = speciesname,
                         filename = filename,
                         short_refs = short_ref_list,
                         trait_files = data_all,
                         write_output = TRUE)
  
{
  extract_val <- function(string2find, trait_file_txt){
    stringr::str_trim(strsplit(trait_file_txt[grepl(string2find,trait_file_txt)],";|=")[[1]][2])
  }
  convert_to_vector <- function(input_string) {
    # Remove curly braces { }
    cleaned_string <- gsub("[{}]", "", input_string)
    
    # Split by comma and remove extra spaces
    string_vector <- trimws(strsplit(cleaned_string, ",")[[1]])
    
    return(string_vector)
  }
  #unpack data 
  
  #mydata file
  trait_file_txt <- unlist(trait_files$mydata)
  
  trait_file_txt <- str_remove_all(trait_file_txt, "'")
  
  dataset_name <- paste0("mydata_",str_remove_all(filename,".zip"))
  
  entry_author <- trait_file_txt[grepl("metaData.author",trait_file_txt)][1]  
  entry_author <- str_extract(entry_author,"(?<=\\{).+?(?=\\})")
  
  entry_curator <-trait_file_txt[grepl("metaData.curator",trait_file_txt)][1] 
  entry_curator <- str_extract(entry_curator,"(?<=\\{).+?(?=\\})")
  
  submitted_date <- trait_file_txt[grepl("metaData.date_subm",trait_file_txt)][1] 
  submitted_date <- gsub(pattern = " ",
                         replacement = "-",
                         x = str_extract(submitted_date,"(?<=\\[).+?(?=\\])"))
  
  complete_score <- trait_file_txt[grepl("metaData.COMPLETE",trait_file_txt)][1]
  complete_score <- gsub(pattern = " ", 
                         replacement = "",
                         str_extract(complete_score,"(?<=\\=).+?(?=\\;)"))
  
  T_typical <- trait_file_txt[grepl("metaData.T_typical",trait_file_txt)][1]  
  T_typical <- str_extract(T_typical,"(?<=\\().+?(?=\\))")
  
  facts <- trait_file_txt[grepl("F[0-9]",trait_file_txt)]
  facts <- paste(facts, collapse = ";")
  
  discussion <- trait_file_txt[grepl("D[0-9]",trait_file_txt)]
  discussion <- paste(discussion, collapse = ";")
  
  source_file<-paste0("http://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries_zip/",filename)
  
  # extract taxonomy
  phylum <- extract_val("metaData.phylum", trait_file_txt)
  class <- extract_val("metaData.class", trait_file_txt)
  order <- extract_val("metaData.order", trait_file_txt)
  family <- extract_val("metaData.family", trait_file_txt)
  genus <- strsplit(extract_val("metaData.species", trait_file_txt), '_')[[1]][1]
  species0 <- strsplit(extract_val("metaData.species", trait_file_txt), '_')[[1]][2]
  species_en <- extract_val("metaData.species_en", trait_file_txt)
  # extract ecocodes
  climate <- paste(convert_to_vector(extract_val("metaData.ecoCode.climate", trait_file_txt)), collapse = ", ")
  ecozone <- paste(convert_to_vector(extract_val("metaData.ecoCode.ecozone", trait_file_txt)), collapse = ", ")
  habitat <- paste(convert_to_vector(extract_val("metaData.ecoCode.habitat", trait_file_txt)), collapse = ", ")
  embryo <- paste(convert_to_vector(extract_val("metaData.ecoCode.embryo", trait_file_txt)), collapse = ", ")
  migrate <- paste(convert_to_vector(extract_val("metaData.ecoCode.migrate", trait_file_txt)), collapse = ", ")
  food <- paste(convert_to_vector(extract_val("metaData.ecoCode.food", trait_file_txt)), collapse = ", ")
  gender <- paste(convert_to_vector(extract_val("metaData.ecoCode.gender", trait_file_txt)), collapse = ", ")
  reprod <- paste(convert_to_vector(extract_val("metaData.ecoCode.reprod", trait_file_txt)), collapse = ", ")
  metadata_taxonomy <- cbind(phylum, class, order, family, genus, species0, species_en,
                             climate, ecozone, habitat, embryo, migrate, food, gender, reprod, T_typical)

  #Function to extract references
  get_refs.f<-function(short_ref, trait_file_txt){
    
    #get relevant text
    l_st<-grep(paste0("bibkey = ",short_ref),trait_file_txt)
    if(length(l_st)==0){
      l_st<-grep(paste0("bibkey = '",short_ref, "'"),trait_file_txt)
    }
    if(length(l_st)==0){
      l_st<-grep(paste0("bibkey='",short_ref, "'"),trait_file_txt)
    }
    if(length(l_st)==0){
      l_st<-grep(paste0("bibkey=",short_ref),trait_file_txt)
    }
    if(length(l_st)==0){
      l_st<-grep(paste0("bibkey= '",short_ref, "'"),trait_file_txt)
    }
    if(length(l_st)==0){
      l_st<-grep(paste0("bibkey= ",short_ref),trait_file_txt)
    }
    l_end<-grep("metaData.biblist",trait_file_txt)
    l_end<-l_end[l_end>l_st][1]
    ref_text<-trait_file_txt[l_st:l_end]
    
    #format appropriately
    
    #ignore formatting with italics for now & accents
    to_remove<-c("emph|\\\\|\\{|\\}|url")
    
    ##Match up styles with those outlined here: https://www.bibtex.com/e/entry-types/
    
    # Create an empty dataframe with specified column names

    col_names <- c('type', 'author', 'year', 'title', 'publisher', 'doi', 'school', 'book_editor', 'pages', 'booktitle', 'volume', 'journal', 'note', 'howpublished')
    fullref <- setNames(data.frame(matrix(ncol = length(col_names), nrow = 0)), col_names)
    fullref[1, ] <- NA
    empty_to_na <- function(x){ if (length(x) == 0) NA else x}
    if(length(grep("type = Book",ref_text))>0|length(grep("type = book",ref_text))>0){
      author <- str_extract(ref_text[grep("author",ref_text)], "(?<=\\{).+?(?=\\},)") 
      author <- str_remove_all(author, to_remove)
      year <- str_extract(ref_text[grep("year",ref_text)], "(?<=\\{).+?(?=\\})") 
      title <-  str_extract(ref_text[grep("title",ref_text)], "(?<=\\{).+?(?=\\})") 
      title <- str_remove_all(title, to_remove)
      publisher <- str_extract(ref_text[grep("publisher",ref_text)], "(?<=\\{).+?(?=\\})") 
      doi <- str_extract(ref_text[grep("doi",ref_text)], "(?<=\\{).+?(?=\\})") 
      if(length(doi)==0){
        doi <- NA
      }
      long_ref<-paste0(author,"(",year,") ",title,",",publisher,",",doi)
      fullref$type <- 'book'
      fullref$author <- empty_to_na(author)
      fullref$year <- empty_to_na(year)
      fullref$title <- empty_to_na(title)
      fullref$publisher <- empty_to_na(publisher)
      fullref$doi <- empty_to_na(doi)
    }
    
    if(length(grep("type = Phdthesis",ref_text))>0|length(grep("type = phdthesis",ref_text))>0|length(grep("type = PhDthesis",ref_text))>0|length(grep("type = PhDThesis",ref_text))>0|length(grep("type = phdthesis",ref_text))>0){
      author <- str_extract(ref_text[grep("author",ref_text)], "(?<=\\{).+?(?=\\},)") 
      author <- str_remove_all(author, to_remove)
      year <- str_extract(ref_text[grep("year",ref_text)], "(?<=\\{).+?(?=\\})") 
      title <-  str_extract(ref_text[grep("title",ref_text)], "(?<=\\{).+?(?=\\},)") 
      title <- str_remove_all(title, to_remove)
      school <- str_extract(ref_text[grep("school",ref_text)], "(?<=\\{).+?(?=\\},)") 
      school <- str_remove_all(school, to_remove)
      doi <- str_extract(ref_text[grep("doi",ref_text)], "(?<=\\{).+?(?=\\})") 
      if(length(doi)==0){
        doi <- NA
      }
      long_ref<-paste0(author,"(",year,") ",title,",",school,",",doi)
      fullref$type <- 'PhdThesis'
      fullref$author <- empty_to_na(author)
      fullref$year <- empty_to_na(year)
      fullref$title <- empty_to_na(title)
      fullref$publisher <- empty_to_na(publisher)
      fullref$doi <- empty_to_na(doi)
    }
    
    if(length(grep("type = Mastersthesis",ref_text))>0|length(grep("type = mastersthesis",ref_text))>0){
      author <- str_extract(ref_text[grep("author",ref_text)], "(?<=\\{).+?(?=\\},)") 
      author <- str_remove_all(author, to_remove)
      year <- str_extract(ref_text[grep("year",ref_text)], "(?<=\\{).+?(?=\\})") 
      title <-  str_extract(ref_text[grep("title",ref_text)], "(?<=\\{).+?(?=\\},)") 
      title <- str_remove_all(title, to_remove)
      school <- str_extract(ref_text[grep("school",ref_text)], "(?<=\\{).+?(?=\\},)") 
      school <- str_remove_all(school, to_remove)
      doi <- str_extract(ref_text[grep("doi",ref_text)], "(?<=\\{).+?(?=\\})") 
      if(length(doi)==0){
        doi <- NA
      }
      long_ref<-paste0(author,"(",year,") ",title,",",school,",",doi)
      fullref$type <- 'MastersThesis'
      fullref$author <- empty_to_na(author)
      fullref$year <- empty_to_na(year)
      fullref$title <- empty_to_na(title)
      fullref$publisher <- empty_to_na(publisher)
      fullref$doi <- empty_to_na(doi)
    }
    
    if(length(grep("type = Incollection",ref_text))>0|length(grep("type = incollection",ref_text))>0|length(grep("type = Proceedings",ref_text))>0|length(grep("type = proceedings",ref_text))>0|length(grep("type = InProceedings",ref_text))>0|length(grep("type = inproceedings",ref_text))>0){
      author <- str_extract(ref_text[grep("author",ref_text)], "(?<=\\{).+?(?=\\},)") 
      author <- str_remove_all(author, to_remove) 
      title <- str_extract(ref_text[grep("title",ref_text)], "(?<=\\{).+?(?=\\})")[1]
      title <- str_remove_all(title, to_remove)
      book_editor <- str_extract(ref_text[grep("editor",ref_text)], "(?<=\\{).+?(?=\\})") 
      year <- str_extract(ref_text[grep("year",ref_text)], "(?<=\\{).+?(?=\\})") 
      pages <- str_extract(ref_text[grep("pages",ref_text)], "(?<=\\{).+?(?=\\})") 
      booktitle <-  str_extract(ref_text[grep("booktitle",ref_text)], "(?<=\\{).+?(?=\\})") 
      booktitle <- str_remove_all(booktitle, to_remove)
      publisher <- str_extract(ref_text[grep("publisher",ref_text)], "(?<=\\{).+?(?=\\})")
      volume <- str_extract(ref_text[grep("volume",ref_text)], "(?<=\\{).+?(?=\\})") 
      doi <- str_extract(ref_text[grep("doi",ref_text)], "(?<=\\{).+?(?=\\})") 
      if(length(doi)==0){
        doi <- NA
      }
      long_ref<-paste0(author,"(",year,") ",title,",in ", book_editor,",",booktitle, ", volume ",volume,",", publisher,", pages ",pages,",",doi)
      fullref$type <- 'Incollection'
      fullref$author <- empty_to_na(author)
      fullref$year <- empty_to_na(year)
      fullref$title <- empty_to_na(title)
      fullref$publisher <- empty_to_na(publisher)
      fullref$book_editor <- empty_to_na(book_editor)
      fullref$booktitle <- empty_to_na(booktitle)
      fullref$publisher <- empty_to_na(publisher)
      fullref$volume <- empty_to_na(volume)
      fullref$doi <- empty_to_na(doi)
    }
    
    if(length(grep("type = Article",ref_text))>0|length(grep("type = article",ref_text))>0){
      author <- str_extract(ref_text[grep("author",ref_text)], "(?<=\\{).+?(?=\\})") 
      year <- str_extract(ref_text[grep("year",ref_text)], "(?<=\\{).+?(?=\\})") 
      title <-  str_extract(ref_text[grep("title",ref_text)], "(?<=\\{).+?(?=\\}\\,)")
      title <- str_remove_all(title, to_remove)
      journal <- str_extract(ref_text[grep("journal",ref_text)], "(?<=\\{).+?(?=\\})") 
      volume <- str_extract(ref_text[grep("volume",ref_text)], "(?<=\\{).+?(?=\\})")
      pages <- str_extract(ref_text[grep("pages",ref_text)], "(?<=\\{).+?(?=\\})")
      doi <- str_extract(ref_text[grep("doi",ref_text)], "(?<=\\{).+?(?=\\})") 
      if(length(doi)==0){
        doi <- NA
      }
      long_ref<-paste0(author,"(",year,") ",title,",",journal,",",volume,":",pages,",",doi)
      fullref$type <- 'Article'
      fullref$author <- empty_to_na(author)
      fullref$year <- empty_to_na(year)
      fullref$title <- empty_to_na(title)
      fullref$journal <- empty_to_na(journal)
      fullref$pages <- empty_to_na(pages)
      fullref$volume <- empty_to_na(volume)
      fullref$doi <- empty_to_na(doi)
    }
    
    if(length(grep("type = Misc",ref_text))>0|length(grep("type = misc",ref_text))>0|length(grep("type = data base",ref_text))>0|length(grep("type = database",ref_text))>0||length(grep("type = Database",ref_text))>0){
      author <- str_extract(ref_text[grep("author",ref_text)], "(?<=\\{).+?(?=\\})") 
      year <- str_extract(ref_text[grep("year",ref_text)], "(?<=\\{).+?(?=\\})") 
      title <-  str_extract(ref_text[grep("title",ref_text)], "(?<=\\{).+?(?=\\}\\,)")
      title <- str_remove_all(title, to_remove)
      note <- str_extract(ref_text[grep("note",ref_text)], "(?<=\\{).+?(?=\\})") 
      howpublished <-str_extract(ref_text[grep("howpublished",ref_text)], "(?<=\\{).+?(?=\\};)") 
      howpublished <-str_remove_all(howpublished, to_remove)
      doi <- str_extract(ref_text[grep("doi",ref_text)], "(?<=\\{).+?(?=\\})") 
      if(length(doi)==0){
        doi <- NA
      }
      long_ref<-paste0(author,"(",year,") ",title,",",howpublished,",",note,",",doi)
      fullref$type <- 'Misc'
      fullref$author <- empty_to_na(author)
      fullref$year <- empty_to_na(year)
      fullref$title <- empty_to_na(title)
      fullref$howpublished <- empty_to_na(howpublished)
      fullref$doi <- empty_to_na(doi)
    }
    if(length(grep("type = Techreport",ref_text))>0|length(grep("type = techreport",ref_text))>0){
      author <- str_extract(ref_text[grep("author",ref_text)], "(?<=\\{).+?(?=\\})") 
      year <- str_extract(ref_text[grep("year",ref_text)], "(?<=\\{).+?(?=\\})") 
      title <-  str_extract(ref_text[grep("title",ref_text)], "(?<=\\{).+?(?=\\}\\,)")
      title <- str_remove_all(title, to_remove)
      note <- str_extract(ref_text[grep("note",ref_text)], "(?<=\\{).+?(?=\\})") 
      howpublished <-str_extract(ref_text[grep("howpublished",ref_text)], "(?<=\\{).+?(?=\\};)") 
      howpublished <-str_remove_all(howpublished, to_remove)
      doi <- str_extract(ref_text[grep("doi",ref_text)], "(?<=\\{).+?(?=\\})") 
      if(length(doi)==0){
        doi <- NA
      }
      long_ref<-paste0(author,"(",year,") ",title,",",howpublished,",",note,",",doi)
      long_ref<-paste0(author,"(",year,") ",title,",",howpublished,",",note,",",doi)
      fullref$type <- 'Techreport'
      fullref$author <- empty_to_na(author)
      fullref$year <- empty_to_na(year)
      fullref$title <- empty_to_na(title)
      fullref$howpublished <- empty_to_na(howpublished)
      fullref$doi <- empty_to_na(doi)
    }
    
    if(length(grep("type = Manual",ref_text))>0|length(grep("type = manual",ref_text))>0){
      author <- str_extract(ref_text[grep("author",ref_text)], "(?<=\\{).+?(?=\\})") 
      year <- str_extract(ref_text[grep("year",ref_text)], "(?<=\\{).+?(?=\\})") 
      title <-  str_extract(ref_text[grep("title",ref_text)], "(?<=\\{).+?(?=\\}\\,)")
      title <- str_remove_all(title, to_remove)
      journal <- str_extract(ref_text[grep("journal",ref_text)], "(?<=\\{).+?(?=\\})")
      pages <- str_extract(ref_text[grep("pages",ref_text)], "(?<=\\{).+?(?=\\})")
      note <- str_extract(ref_text[grep("note",ref_text)], "(?<=\\{).+?(?=\\})") 
      howpublished <-str_extract(ref_text[grep("howpublished",ref_text)], "(?<=\\{).+?(?=\\};)") 
      howpublished <-str_remove_all(howpublished, to_remove)
      doi <- str_extract(ref_text[grep("doi",ref_text)], "(?<=\\{).+?(?=\\})") 
      if(length(doi)==0){
        doi <- NA
      }
      long_ref<-paste0(author,"(",year,") ",title,",",journal,",",pages,",",howpublished,",",note,",",doi)
      long_ref<-paste0(author,"(",year,") ",title,",",howpublished,",",note,",",doi)
      fullref$type <- 'Manual'
      fullref$author <- empty_to_na(author)
      fullref$year <- empty_to_na(year)
      fullref$title <- empty_to_na(title)
      fullref$journal <- empty_to_na(journal)
      fullref$howpublished <- empty_to_na(howpublished)
      fullref$doi <- empty_to_na(doi)
    }
    
    if(length(grep("type = Unpublished",ref_text))>0|length(grep("type = unpublished",ref_text))>0){
      author <- str_extract(ref_text[grep("author",ref_text)], "(?<=\\{).+?(?=\\})") 
      year <- str_extract(ref_text[grep("year",ref_text)], "(?<=\\{).+?(?=\\})") 
      title <-  str_extract(ref_text[grep("title",ref_text)], "(?<=\\{).+?(?=\\}\\,)")
      title <- str_remove_all(title, to_remove)
      journal <- str_extract(ref_text[grep("journal",ref_text)], "(?<=\\{).+?(?=\\})")
      pages <- str_extract(ref_text[grep("pages",ref_text)], "(?<=\\{).+?(?=\\})")
      note <- str_extract(ref_text[grep("note",ref_text)], "(?<=\\{).+?(?=\\})") 
      howpublished <-str_extract(ref_text[grep("howpublished",ref_text)], "(?<=\\{).+?(?=\\};)") 
      howpublished <-str_remove_all(howpublished, to_remove)
      doi <- str_extract(ref_text[grep("doi",ref_text)], "(?<=\\{).+?(?=\\})") 
      if(length(doi)==0){
        doi <- NA
      }
      long_ref<-paste0(author,"(",year,") ",title,",",journal,",",pages,",",howpublished,",",note,",",doi)
      fullref$type <- 'Unpublished'
      fullref$author <- empty_to_na(author)
      fullref$year <- empty_to_na(year)
      fullref$title <- empty_to_na(title)
      fullref$journal <- empty_to_na(journal)
      fullref$pages <- empty_to_na(pages)
      fullref$howpublished <- empty_to_na(howpublished)
      fullref$doi <- empty_to_na(doi)
    }
    
    #return(list(long_ref = long_ref, as.list(fullref)))
    return(fullref)
    
  }

  fullrefs <- lapply(short_refs, get_refs.f, trait_file_txt)
  
  fullrefs_df <- do.call(rbind, lapply(fullrefs, as.data.frame))
  #long_ref_df <- long_ref_df %>% dplyr::distinct(title, .keep_all = TRUE) # avoid occasional duplicates
  
  # Combine with short_ref_list
  ref_table <- cbind(short_ref = short_refs, fullrefs_df)
  #colnames(ref_table)<-c("bibkey", "reference", "year", "title", "doi")
  
  #return output as a list
  
  metadata.ls<-list("dataset_name" = dataset_name,
                    "author" = entry_author,
                    "curator" = entry_curator,
                    "submitted_date" = submitted_date,
                    "complete_score" = complete_score,
                    "source_file" = source_file,
                    "facts" = facts,
                    "discussion" = discussion,
                    ref_table = ref_table)
  
  metadata_main <-as.data.frame(cbind(speciesname, entry_author, entry_curator, submitted_date, complete_score, facts, discussion))
  
  if(isTRUE(write_output)){ #if save output = TRUE, generate dataset with all species
    
    file_path <- "data/DEB_species_metadata.csv"
    if (!file.exists(file_path)) {
      # File doesn't exist: write with column names
      write.csv(metadata_main, file_path, row.names = FALSE)
    } else {
      # File exists: append without column names
      write.table(metadata_main, file_path, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    }
    file_path <- "data/taxonomy.csv"
    if (!file.exists(file_path)) {
      # File doesn't exist: write with column names
      write.csv(metadata_taxonomy, file_path, row.names = FALSE)
    } else {
      # File exists: append without column names
      write.table(metadata_taxonomy, file_path, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    }
    #write.table(metadata_main,file = "data/DEB_species_metadata.csv", sep = ",",col.names = FALSE, row.names = FALSE, append = TRUE)
    #write.table(metadata_taxonomy,file = "data/taxonomy.csv", sep = ",",col.names = FALSE, row.names = FALSE, append = TRUE)

  }
  
  return(metadata.ls)
}
#Reads the .xlsx file provided by the user
read_raw_tables <- function(input, output, session){
  reactive({
    
    #Do nothing if no table is available
    inFile <- input$file1
    if(is.null(inFile)){
      return(NULL)
    }
    
    #Read each table in the .xlsx file
    tables <- list()
    tables$samples <- as.data.frame(read_excel(path = inFile$datapath,
                                               sheet = 1,
                                               trim_ws = T,
                                               skip = 1))
    tables$isolation <- as.data.frame(read_excel(path = inFile$datapath,
                                                 sheet = 2,
                                                 trim_ws = T,
                                                 skip = 1))
    tables$taxonomy <- as.data.frame(read_excel(path = inFile$datapath,
                                                sheet = 3,
                                                trim_ws = T,
                                                skip = 1))
    tables$literature <- as.data.frame(read_excel(path = inFile$datapath,
                                                  sheet = 4,
                                                  trim_ws = T,
                                                  skip = 1))
    tables$assays <- as.data.frame(read_excel(path = inFile$datapath,
                                              sheet = 5,
                                              trim_ws = T,
                                              skip = 1))
    
    #Remove double columns
    tables$literature$Microorganism_scientific_name <- NULL
    tables$assays$Microorganism_scientific_name <- NULL
    
    #Join the tables into one
    tables$total <-full_join(tables$assays, tables$literature)
    tables$total <- full_join(tables$total, tables$taxonomy)
    tables$total <- left_join(tables$total, tables$isolation)
    tables$total <- left_join(tables$total, tables$samples)
    return(tables)
  })
}

#Subsets the tables according to the values specified by the user under the "Filters" menu item.
subset_tables <- function(input, output, session, raw_tables){
  eventReactive(c(raw_tables(), input$filter_button), {
    
    #Do nothing if no tables are available
    if (is.null(raw_tables())){
      return(NULL)
    }
    
    #Create empty datastructures
    data <- list()
    remove_ids <- vector()
    
    #Plant pathogen filtering
    if (input$radio_plant == "No"){
      remove_ids <- subset(raw_tables()$literature, tolower(Pathogenicity_plant) == "yes")$Microorganism_ID
    }
    
    #Human pathogen filtering
    remove_ids <- c(remove_ids,
                    subset(raw_tables()$literature, Pathogen_class > input$slider_patho)$Microorganism_ID)
    
    #PGP function filtering
    for (pgp in input$checkbox_pgp){
      remove_ids <- c(remove_ids, 
                      raw_tables()$assays[tolower(raw_tables()$assays[[pgp]]) == "no",]$Microorganism_ID)
    }
    
    #Fungal biocontrol filtering
    fungi <- c("Pe", "Bo", "Mo", "Geo", "Fu")
    for (fung in fungi){
      filter_id <- paste("slider_", fung, sep = "")
      remove_ids <- c(remove_ids, 
                      raw_tables()$assays[raw_tables()$assays[[fung]] < input[[filter_id]],]$Microorganism_ID)
    }
    
    #Bacterial biocontrol filtering
    bacts <- c("Pse", "Xa", "Agr")
    for (bact in bacts){
      filter_id <- paste("slider_", bact, sep = "")
      remove_ids <- c(remove_ids, 
                      raw_tables()$assays[raw_tables()$assays[[bact]] < input[[filter_id]],]$Microorganism_ID)
    }
    
    #Microorganism type filtering
    filter_id <- data.frame(id = c("Prokaryotes (16S)", "Fungi (ITS)"),
                            pattern = c("16S", "ITS"), stringsAsFactors = F)
    for (id in filter_id$id){
      if (!id %in% input$checkbox_type){
        pattern <- filter_id$pattern[which(filter_id$id == id)]
        index <- grep(pattern, raw_tables()$taxonomy$Amplificiation)
        remove_ids <- c(remove_ids,
                        raw_tables()$taxonomy$Microorganism_ID[index])
      }
    }
    
    #Microorganism growth filtering
    filter_id <- data.frame(id = c("Epiphytic", "Endophytic"),
                            pattern = c("Epifito", "Endofito"), stringsAsFactors = F)
    for (id in filter_id$id){
      if (!id %in% input$checkbox_xphyte){
        pattern <- filter_id$pattern[which(filter_id$id == id)]
        index <- grep(pattern, raw_tables()$isolation$Microorganism_type)
        remove_ids <- c(remove_ids,
                        raw_tables()$isolation$Microorganism_ID[index])
      }
    }
    
    #Subset data tables based on the IDs that have been selected for removal
    data$samples <- raw_tables()$samples
    data$isolation <- raw_tables()$isolation[!raw_tables()$isolation$Microorganism_ID %in% remove_ids,]
    data$taxonomy <- raw_tables()$taxonomy[!raw_tables()$taxonomy$Microorganism_ID %in% remove_ids,]
    data$literature <- raw_tables()$literature[!raw_tables()$literature$Microorganism_ID %in% remove_ids,]
    data$assays <- raw_tables()$assays[!raw_tables()$assays$Microorganism_ID %in% remove_ids,]
    
    #Generate a joined table of all relevant information
    data$total <- full_join(data$assays, data$literature, by = c("Microorganism_ID"))
    data$total <- full_join(data$total, data$taxonomy, by = c("Microorganism_ID"))
    data$total <- left_join(data$total, data$isolation, by = c("Microorganism_ID"))
    data$total <- left_join(data$total, data$samples, by = c("Sample_ID"))
    return(data)
  })
}
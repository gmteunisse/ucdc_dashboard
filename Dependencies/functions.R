## Title:   Functions for shiny dashboard for collection analysis
## Date:    19-oct-2017
## Author:  Guus Martijn Teunisse (gmteunisse@gmail.com)
##########################################################################

###### Functions #####

#Loads your packages and installs those that are not yet installed
awesome_package_loader <- function(package_vector){
  for(package_name in package_vector){
    require_code <- paste("require(", package_name, ")", sep="")
    if (!eval(parse(text = require_code))){
      install_code <- paste("install.packages('", package_name, "')", sep="")
      eval(parse(text = install_code))
      eval(parse(text = require_code))
    }
  }
}

#Generates a donut plot for the distribution of "yes", "no" and "not available"
plot_freq <- function(data, variable){
  
  #Count occurrences and make ready for plotting. Not using table() function because not suitable.
  df <- data$total
  tbl <- data.frame(Var1 = factor(x = c("Yes", "No", "Not available"),
                                  levels = c("Yes", "No", "Not available"),
                                  ordered = T),
                    Freq = c(0, 0, 0))
  tbl[1,2] <- sum(tolower(df[[variable]]) == "yes", na.rm = T)
  tbl[2,2] <- sum(tolower(df[[variable]]) == "no", na.rm = T)
  tbl[3,2] <- sum(is.na(df[[variable]]))
  tbl$Freq[tbl$Freq == 0] <- NA
  colors <- c(Yes = color_orange[3], No = color_blue[3], "Not available" = color_grey[3])
  
  #Create plot
  p <- ggplot(tbl, aes(x = "", y = Freq, fill = Var1, group = Var1)) +
    geom_bar(stat = "identity", width = 0.3) +
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3) +
    coord_polar("y") +
    scale_fill_manual(values = colors) +
    plot_theme +
    theme(legend.title=element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.background = element_blank(),
          panel.grid.major.y = element_blank(),
          text = element_text(family = "Helvetica", size = 12),
          legend.position = "top",
          legend.direction = "vertical")
  
  return(p)
  
}

#Generates a donut plot for the distribution of identified genera.
plot_genera <- function(data, region){
  
  #Subset to 16S data
  df <- data$total[grep(pattern = region, x = data$total$Amplificiation),]
  if(nrow(df) < 1){
    p <- NULL
    return(p)
  }
  tbl <- as.data.frame(table(df$Genus, useNA = "ifany"), stringsAsFactors = F)
  
  #Make ready for plotting: Sort, move NAs to end and define colors
  if (nrow(tbl) < 15){n <- 15}else{n <- nrow(tbl)}
  color <- rep(c(color_blue, color_tan, color_orange), n/15)[1:nrow(tbl)]
  tbl <- tbl[order(tbl$Freq, decreasing = T),]
  if (sum(is.na(tbl$Var1)) > 0){
    na_index <- which(is.na(tbl$Var1))
    tbl[nrow(tbl) + 1,] <- c("Not Available", tbl[na_index,]$Freq)
    tbl <- tbl[-na_index,]
    color[nrow(tbl)] <- color_grey[3]
  }
  tbl$Var1 <- factor(x = tbl$Var1, levels = tbl$Var1, ordered = T)
  
  #Create plot
  p <- ggplot(tbl, aes(x = "", y = Freq, fill = Var1, group = Var1)) +
    geom_bar(stat = "identity", position = "stack", width = 0.3) +
    coord_polar("y") +
    scale_fill_manual(values = color) +
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3) +
    plot_theme +
    theme(legend.title=element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.spacing.x = unit(-0.5,"cm"),
          panel.spacing.y = unit(-0.5,"cm"),
          text = element_text(family = "Helvetica", size = 12))
  
  return(p)
}

plot_assay_dot <- function(data, raw_tables, cols){
  df <- raw_tables$total
  df$Status <- "Filtered"
  index <- which(df$Microorganism_ID %in% data$total$Microorganism_ID)
  df$Status[index] <- "Selected"
  df$Status <- factor(x = df$Status, levels = c("Selected", "Filtered"), ordered = T)
  df <- subset(x = df, select = c("Microorganism_ID", "Status", cols))
  df.long <- melt(df, id.vars = c("Microorganism_ID", "Status"))
  df.long$value <- as.numeric(df.long$value)
  df.long <- df.long[order(x = df.long$value, decreasing = T),]
  df.long$Microorganism_ID <- factor(x = df.long$Microorganism_ID, levels = unique(df.long$Microorganism_ID), ordered = T)
  colors <- c(color_blue[3], color_grey[3])
  set.seed(1)
  p <- ggplot(df.long, aes(x = Microorganism_ID,
                           y = value, 
                           color = Status, 
                           group = Status,
                           label = Microorganism_ID)) +
    geom_point() +
    scale_y_continuous(limits = c(0, 100),
                       expand = c(0, 0), 
                       labels = function(x){paste(x, "%", sep = "")}) + 
    scale_color_manual(values = colors) +
    plot_theme +
    theme(
      axis.line.y = element_blank(),
      axis.title.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      legend.position="none"
    )
  p <- ggplotly(p, tooltip = c("label", "value"))
  return(p)
  
}

plot_assay_hist <- function(data, cols){
  
  df <- subset(x = data$total, select = c("Microorganism_ID", cols))
  df.long <- melt(df, id.vars = c("Microorganism_ID"))
  p <- ggplot(df.long, aes(value, fill = variable, group = variable)) +
    geom_histogram(bins = 50, position = "dodge", center = 1) +
    scale_x_continuous(limits = c(-2, 100), labels = function(x){paste(x, "%", sep = "")}) +
    scale_y_continuous(expand = c(0, 0)) + 
    scale_fill_manual(values = color_blue[3]) +
    plot_theme +
    theme(
      axis.line.y = element_blank(),
      axis.title.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      legend.position="none"
    )
  
  return(p)
}

plot_heatmap <- function(data){
  
  pgp_assays <- c("PA", "PS", "SF", "FN")
  bctrl_assays <- c("Pe", "Bo", "Mo", "Geo", "Fu", "Pse", "Xa", "Agr")
  df <- subset(data$total, select = c("Microorganism_ID", pgp_assays, bctrl_assays))
  df <- melt(df, id.vars = "Microorganism_ID")
  df$group <- NA
  df$group[which(df$variable %in% pgp_assays)] <- "PGP"
  df$group[which(df$variable %in% bctrl_assays)] <- "Biocontrol"
  df$value[which(df$value == "yes")] <- 1
  df$value[which(df$value == "no")] <- 0
  df$variable <- as.factor(df$variable)
  df$value <- as.numeric(df$value)
  max_values <- aggregate(value ~ variable, df, max, na.rm = T)
  df$adj_value <- df$value / max_values$value[match(df$variable, max_values$variable)]
  score <- aggregate(adj_value ~ Microorganism_ID, df, sum, na.rm = T)
  score <- score[order(score$adj_value, decreasing = F),]
  df$Microorganism_ID <- factor(x = df$Microorganism_ID, levels = score$Microorganism_ID, ordered = T)
  df <- df[!is.na(df$Microorganism_ID),]
  
  p <- ggplot(df, aes(x = variable,
                      y = Microorganism_ID,
                      fill = adj_value,
                      label = value)) +
    geom_tile() +
    facet_grid(~group, scales = "free_x", space = "free_x") +
    scale_fill_continuous(low = "#eff4fd",
                          high = color_blue[3],
                          na.value = "#FFFFFF") +
    scale_x_discrete(position = "top") +
    geom_text(color = "#2e2e2e" , size = 3) +
    plot_theme +
    theme(
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      panel.grid.major = element_blank(),
      legend.position="none"
    )
  return(p)
}

#Generates a donut plot for the distribution of human pathogenicity levels
plot_path <- function(data){
  
  #Count occurrences and make ready for plotting. Not using table() function because not suitable.
  df <- data$total
  tbl <- data.frame(Var1 = factor(x = c(1, 2, 3, 4, "Not available"),
                                  levels = c(1, 2, 3, 4, "Not available"),
                                  ordered = T),
                    Freq = c(0, 0, 0, 0, 0))
  tbl[1,2] <- sum(df[["Pathogen_class"]] == "1", na.rm = T)
  tbl[2,2] <- sum(df[["Pathogen_class"]] == "2", na.rm = T)
  tbl[3,2] <- sum(df[["Pathogen_class"]] == "3", na.rm = T)
  tbl[4,2] <- sum(df[["Pathogen_class"]] == "4", na.rm = T)
  tbl[5,2] <- sum(is.na(df[["Pathogen_class"]]))
  tbl$Freq[tbl$Freq == 0] <- NA
  colors <- c("1" = color_blue[3], 
              "2" = color_orange[3],
              "3" = color_orange[2],
              "4" = color_orange[1],
              "Not available" = color_grey[3])
  
  #Create plot
  p <- ggplot(tbl, aes(x = "", y = Freq, fill = Var1, group = Var1)) +
    geom_bar(stat = "identity", width = 0.3) +
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3) +
    coord_polar("y") +
    scale_fill_manual(values = colors) +
    plot_theme +
    theme(legend.title=element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.background = element_blank(),
          panel.grid.major.y = element_blank(),
          text = element_text(family = "Helvetica", size = 12),
          legend.position = "top",
          legend.direction = "vertical")
  
  return(p)
  
}

fill_df <- function(filelist, df, category)
{
  for(i in 1:nrow(filelist))
  {
      i_df <- read.csv(filelist[i,"datapath"])   
      df_row <- data.frame(t(i_df[category]), row.names=NULL)
      colnames(df_row) <-  t(i_df["CellID"])
      
      df_row <- cbind(Image = str_replace(filelist[i,"name"], ".csv", ""), df_row)
      df <- merge(df, df_row, all.x = TRUE, all.y= TRUE)
    
  }
  return(df)
}
# Group individual experiment types, aggregate, and output
group_types <- function(df, groups)
{
  #completed <- c()
  
  
  df_new = data.frame(Image="df new placeholder") 
  
  aggregated = vector("list", nrow(df))
  for(group in groups)
  {
    df_row = data.frame(Image = "df row placeholder") 
    rows = which(grepl(group, df[["Image"]], fixed = TRUE))

    
    #completed <- c(completed, rows)
    
    for(row in rows)
    {
      
      df_row <- cbind(df_row,df[row,])
     # df[row, "Image"] <- "used"
      }
    df_row <- df_row[, !(colnames(df_row) %in% c("Image"))]
    df_row <- df_row[ , colSums(is.na(df_row)) == 0]
    colnames(df_row)<- 1:ncol(df_row)
    df_row["Image"] = group
    
    df_new <- merge(df_new, df_row, all.x= TRUE, all.y = TRUE)
  }
  df_new <- df_new[-1,]
  
 # completedtest<<-completed
  
  #if(all(completed == test))
  #  print("SORT SUCCESSFUL")
  #else
   # print("ERROR SORTING")
  return(df_new)
}

# Sort all data from computed images.  Aggregate, output
sort_data <- function(res, groups)
{

  groups <- format_groups(groups)
  
  
  df_pm_mpi <- data.frame(Image = character(),stringsAsFactors=FALSE)
  df_vac_mpi <- data.frame(Image = character(),stringsAsFactors=FALSE)
  df_pm_vac_ratio <- data.frame(Image = character(),stringsAsFactors=FALSE)
  
  
  
  
  df_pm_mpi <- fill_df(res,df_pm_mpi, "cell_mpi")
  df_vac_mpi <- fill_df(res,df_vac_mpi, "vac_mpi")
  df_pm_vac_ratio <- fill_df(res,df_pm_vac_ratio, "PM_vac_ratio")

  
  df_pm_mpi_grouped <- group_types(df_pm_mpi, groups)
  df_vac_mpi_grouped <- group_types(df_vac_mpi, groups)
  df_pm_vac_ratio_grouped <- group_types(df_pm_vac_ratio, groups)
  

  df_pm_mpi <- t(df_pm_mpi)
  df_vac_mpi <- t(df_vac_mpi)
  df_pm_vac_ratio <- t(df_pm_vac_ratio)
  
  df_pm_mpi_grouped <- t(df_pm_mpi_grouped)
  df_vac_mpi_grouped <- t(df_vac_mpi_grouped)
  df_pm_vac_ratio_grouped <- t(df_pm_vac_ratio_grouped)
  
  
  print("Finished sorting data.")
  
  
  list(pm_mpi = df_pm_mpi,
       vac_mpi = df_vac_mpi,
       pm_vac = df_pm_vac_ratio,
       grouped_pm_mpi = df_pm_mpi_grouped,
       grouped_vac_mpi = df_vac_mpi_grouped,
       grouped_ratio = df_pm_vac_ratio_grouped)
}










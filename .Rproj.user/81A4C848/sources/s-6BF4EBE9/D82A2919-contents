get_table <- function(data)
{
  data <- data["name"]
  names(data) <- "Files"
  return(data)
}

format_groups <- function(group_list)
{
  group_list <- str_replace(group_list, "\\n", "")
  
  group_list <- strsplit(group_list, ",")

  group_list <- unlist(group_list)
  
  group_list <- lapply(group_list, function(g){str_trim(g)})
  
  
  return(group_list)
}

sort_table <- function(data, groups)
{

  data<- data["name"]
  names(data) <- "Files"
  data["Group"] = ""
  
  groups <- format_groups(groups)
 
  
  #inputgroups <<- groups
  #print(groups)
  
  for(group in groups)
  {
    rows = which(grepl(group, data[,"Files"], fixed = TRUE))
    for(row in rows)
    {
      data[row, "Group"] = group
    }
  }
  
  b<-c("#FBB4AE","#B3CDE3" )
  if(length(groups)>= 3)
    b<-brewer.pal(n = length(groups), name = "Pastel1")
  else if(length(groups) == 1)
  {
    b<-b[1]
  }
  else if(length(groups) == 2)
  {
    b<-c(b[1], b[2])
  }
  
  res<-datatable(data, options = list(dom = 'tpl', pageLength = 15)) %>% 
    formatStyle('Group',
                target = 'row',
                backgroundColor = styleEqual(groups, b))
  return(renderDataTable({res}))
}


get_graph <- function(res, groups, title)
{
  
  groups <- format_groups(groups)
  
  
  
  df_pm_vac_ratio <- data.frame(Image = character(),stringsAsFactors=FALSE)
  df_pm_vac_ratio <- fill_df(res,df_pm_vac_ratio, "PM_vac_ratio")
  df_pm_vac_ratio_grouped <- group_types(df_pm_vac_ratio, groups)
  df <- t(df_pm_vac_ratio_grouped)

  
  beforemelt <<- df
  
  rownames(df) <- c()
  colnames(df) <- df[1,]
  df<-df[-1,]
  
  df<-as.data.frame(df)
  df[] <- lapply(df, function(x) as.numeric(as.character(x)))
  

  res_graph <- ggplot(data = melt(df), aes(x = variable, y = value))+
    geom_dotplot(binaxis="y", stackdir = "center", stackratio = 0.5, binwidth=0.01, alpha=0.5, aes(color=variable), dotsize = 1.5) + 
    xlab("\nGroup") + 
    ylab("PM/vac ratio") + 
    ggtitle(paste0(title, " PM/vac ratio")) +
    stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="errorbar", color="black", width=0.5) +
    stat_summary(fun.y="mean", geom="point", color="black") +scale_x_discrete() + 
    theme(legend.position = "none",axis.text.x=element_text(angle = -45, hjust = 0))
  
  
  return(res_graph)

  
  
  
  
}
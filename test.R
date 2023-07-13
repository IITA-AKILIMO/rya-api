
#* @post /async_sqrt
#* @param n
function(n) {
  future::future({
    #Sys.sleep(5)
    x<-sqrt(as.numeric(n))
    x
  })
}
#* @get /not_async_sqrt
#* @param n
function(n) {
  Sys.sleep(5)
  x<-sqrt(as.numeric(n))
  x
}
#* @get /sqrt
#* @param n
function(n) {
  x<-sqrt(as.numeric(n))
  x
}


RYA_plots <- function(results, field_area,file_name,ext) {
  
  #SHORT DEF:   Make plots visualizing production estimate and uncertainty
  #RETURNS:     List of 2 ggplot objects. 
  #             The first is a simple visualization of the production estimate and confidence intervals (80% and 95%)
  #             The second also overlays 5 yield classes
  #DESCRIPTION: The function creates the 2 graphs that are used in the mock-up app.
  #INPUT:       results: vector of 5 elements: LL 95%CI, LL 80% CI, estimate, UL 80% CI, UL 95%CI
  #             field_area: area of the field in m?
  
  
  colors <- c(Low="#FFF5CD",Common="#FFD629",Medium="#C49F00",High="#745E00",`Very high`="#261F00")
  
  est <- results[3]
  x <- seq(round(results[1],0)-3,round(results[5],0)+3, by=2)
  
  df <- data.frame(prod=x)
  
  # Simple plot of production estimate
  p1 <- ggplot(data=df, aes(x=prod, y=1)) +
    geom_rect(xmin=results[2], xmax=results[4], ymin=0.985, ymax=0.995, fill="#F0E442") +
    geom_rect(xmin=results[1], xmax=results[5], ymin=0.975, ymax=0.985, fill="#56B4E9") +
    geom_text(aes(label=round(prod,0)), show.legend=FALSE, size=5) +
    geom_text(label="80%", x=(est+results[4])/2, y=0.99, size=7) +
    geom_text(label="90%", x=(est+results[5])/2, y=0.98, size=7) +
    geom_point(x=est, y=0.9925, pch=17, size=3, col="red") +
    geom_text(label="Production (tonnes)", x=est, y=1.01, size=8) +
    theme_void()
  
  
  # Plot with yield classes 
  yield_classes <- c(round(c(0,7.5,15,22.5,30, 40) * field_area, 2))
  
  print(yield_classes)
  df$text_col <- ifelse(df$prod < yield_classes[4], "black","white")
  
  p2 <- ggplot(data=df, aes(x=prod, y=1)) +
    
    geom_rect(xmin=results[2], xmax=results[4], ymin=0.99, ymax=0.995, fill="#F0E442") +
    geom_rect(xmin=results[1], xmax=results[5], ymin=0.985, ymax=0.99, fill="#56B4E9") +
    
    geom_rect(xmin=yield_classes[1], xmax=yield_classes[2], 
              ymin=0.996, ymax=1.004, aes(fill="Low")) +
    geom_rect(xmin=yield_classes[2], xmax=yield_classes[3], 
              ymin=0.996, ymax=1.004, aes(fill="Common")) +
    geom_rect(xmin=yield_classes[3], xmax=yield_classes[4], 
              ymin=0.996, ymax=1.004, aes(fill="Medium")) +
    geom_rect(xmin=yield_classes[4], xmax=yield_classes[5], 
              ymin=0.996, ymax=1.004, aes(fill="High")) +
    geom_rect(xmin=yield_classes[5], xmax=yield_classes[6], 
              ymin=0.996, ymax=1.004, aes(fill="Very high")) +
    
    geom_text(label="80%", x=(est+results[4])/2, y=0.9925, size=7) +
    geom_text(label="95%", x=(est+results[5])/2, y=0.9875, size=7) +
    
    
    geom_text(aes(label=round(prod,0), col=text_col), show.legend=FALSE, size=5) +
    geom_point(x=est, y=0.9925, pch=17, size=3, col="red") +
    geom_text(label="Production (tonnes)", x=est, y=1.01, size=8) +
    theme_void() +
    theme(legend.position=c(0.5,0.2), 
          legend.direction="horizontal") +
    scale_fill_manual(name="Yield classes",
                      values=colors, guide=guide_legend(title.position = "bottom",
                                                        title.hjust=0.5,
                                                        title.theme=element_text(face="bold"))) +
    scale_color_manual(values=c("white"="white","black"="black")) +
    coord_cartesian(ylim=c(0.97,1.03))     
  
  ggsave(file=paste("images/",file_name,ext,sep = ''), plot=p1)
  ggsave(file=paste("images/",file_name,"2",ext,sep = ''), plot=p2)
  
  
  return(list(p1,p2))
}


#* @param results
#* @param field_area
#* @param file_name
#* @param ext
#* @param read
#* 
#* Functions that make the required graphics
#* 
#* 
#* @serializer json
#* @post /plot
generate_plots <- function(results,field_area,file_name,ext=".png",read=FALSE)
{
  result <- rjson::fromJSON(results)
  plots = RYA_plots(results = result, field_area = field_area,file_name=file_name,ext = ext)
  
  return (list(file_name,ext))
}

#* @param file_name
#* @param ext
#* Functions that make the required graphics
#* 
#* 
#* @serializer contentType list(type='image/*')
#* @get /read-plot
generate_plots <- function(file_name,ext=".png")
{
  
  theFile <-paste("images/",file_name,ext,sep = '')
  print(theFile)
  print(file.info(theFile)$size)
  readBin(theFile, "raw", n = file.info(theFile)$size)
}
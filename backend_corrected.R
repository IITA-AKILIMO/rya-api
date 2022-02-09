
## Dries De Bi?vre


RYA_estimate <- function(plant_counts=c(), 
                         plant_RMs=c(), 
                         triangle_RMs=c(), 
                         precision_type, 
                         field_area, 
                         L=5){
  #SHORT DEF:   Estimate production and limits of 80% and 95% CIs
  #RETURNS:     Vector of 5 elements: LL 95%CI, LL 80% CI, estimate, UL 80% CI, UL 95%CI
  #DESCRIPTION: The function estimates production based on root mass data and density measurements
  #             for 3 methods which differ in precision. Each method can consist of any number of triangles placed.
  #             Under low precision, 3 plants are uprooted per triangle and their root mass is assessed per plant.
  #             Under medium precision, 6 plants are uprooted per triangle and root mass is assessed per plant.
  #             Under high precision, all plants in each triangle are uprooted. Root mass is measured per triangle, 
  #             though if the balance does not allow to weigh the roots bulked, they can be given per plant.
  #INPUT:       plant_counts: vector of the number of plants in each triangle, not required when precision_type="high"
  #             plant_RMs: vector of root masses per plant for the low or medium precision OR 
  #                        list of root masses per plant, with plants from different triangles in different list elements for high precision
  #             triangle_RMs: vector of root masses per triangle, when bulking roots per triangle in high precision method.
  #             precision_type: string, 1 of 3 options: "low","medium" or "high".
  #             field_area: area of the field in m?
  #             L: side of the triangle (5 by default)
  
  
  triA <- sqrt(L**2 - (L/2)**2) * (L/2) # calculates the are of the triangle
  
  
  # Check if input data is correct is correct given the precision type
  
  if (precision_type=="low" & length(plant_RMs) != 3*length(plant_counts)) {
    stop("With the 'low' precision method, you have to uproot 3 plants per triangle")
  }
  
  if (precision_type=="medium" & length(plant_RMs) != 6*length(plant_counts)) {
    stop("With the 'medium' precision method, you have to uproot 6 plants per triangle")
  }
  
  if (precision_type=="high" & is.null(triangle_RMs) & !is.list(plant_RMs)) {
    stop("plant_RMs should be given as a list with an element per triangle")
  }
  
  if (precision_type=="high" & is.null(triangle_RMs) & is.null(plant_RMs)) {
    stop("Either give root mass of each plant individually or for each triangle bulked")
  }
  
  
  # Estimate production when precision_type is low or medium
  if (precision_type %in% c("low","medium")) {
    
    df <- data.frame(avg_dens=mean(plant_counts/triA), # plants/m?
                     avg_RM=mean(plant_RMs), #kg/plant
                     dens_var=var(plant_counts/triA),
                     RM_var=var(plant_RMs),
                     tot_plants = length(plant_RMs),
                     nrTri = length(plant_counts))
    
    est <- with(df, avg_dens*avg_RM*10)
    var_est <- with(df, ((dens_var/nrTri+avg_dens**2)*(RM_var/tot_plants + avg_RM**2)-(avg_dens*avg_RM)**2) *100)
    
  }
  
  # Estimate production when precision_type is high and roots were bulked per triangle
  if (precision_type == "high" & !is.null(triangle_RMs) & is.null(plant_RMs)) {
    
    est <- mean(triangle_RMs/triA)*10 # ton/ha
    var_est <- var(triangle_RMs/triA)*100
  }
  
  # Estimate production when precision_type is high and roots were weighed per plant
  if (precision_type == "high" & is.list(plant_RMs) & is.null(triangle_RMs)) {
    triangle_RMs <- sapply(plant_RMs, sum)
    est <- mean(triangle_RMs/triA)*10 # ton/ha
    var_est <- var(triangle_RMs/triA)*100
    
  }
  
  sd_est <- sqrt(var_est)
  CI <- qnorm(c(0.025,0.10,0.5,0.90,0.975), mean=est, sd=sd_est)
  CI_prod <- CI*field_area
  return(CI_prod)
}


# TEST low precision method

test_c <- c(10,12,15)
test_rm <- c(1.5,1.2,1.6,1.9,0.9,1.6,1.3,2,1.4)

RYA_estimate(plant_counts=test_c,plant_RMs=test_rm, precision_type="low", field_area=2)
# OUT: 24.57436 27.81128 33.92596 40.04064 43.27755

# TEST high precision, triangle yields bulked
triangle_yields <- c(17,13,16)
RYA_estimate(plant_counts=test_c, triangle_RMs=triangle_yields, precision_type="high", field_area=2)
# OUT: 20.79079 23.39991 28.32865 33.25740 35.86652

# TEST high precision, roots weighed per plant, input is list
tri1 <- c(1.5,1.2,1.6,1.9,0.9,1.6,1.3,2,1.4, 1.8)
tri2 <- c(0.5,1.8,1.4,1.4,1.6,1.3,1.8,1.5,1.6,2.3,1.2,1.7)
tri3 <- c(1.3,1.9,1.6,1.4,1.2,1.4,1.6,1.7,2)

test_plants <- list(tri1, tri2, tri3)

RYA_estimate(plant_RMs=test_plants, precision_type="high", field_area=2)
# OUT: 21.70825 24.29823 29.19083 34.08343 36.67341

## Functions that make the required graphics

RYA_plots <- function(results, field_area) {
  
  #SHORT DEF:   Make plots visualizing production estimate and uncertainty
  #RETURNS:     List of 2 ggplot objects. 
  #             The first is a simple visualization of the production estimate and confidence intervals (80% and 95%)
  #             The second also overlays 5 yield classes
  #DESCRIPTION: The function creates the 2 graphs that are used in the mock-up app.
  #INPUT:       results: vector of 5 elements: LL 95%CI, LL 80% CI, estimate, UL 80% CI, UL 95%CI
  #             field_area: area of the field in m?
  
  require(ggplot2)
  
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
  
  require('svglite')
  ggsave(file="Scatter1.svg", plot=p1, width=10, height=10)
  ggsave(file="Scatter.svg", plot=p2, width=10, height=10)
  return(list(p1,p2))
}

# TEST
test_c <- c(10,12,16)
test_rm <- rnorm(3*3, 1.5,0.7)

res <- RYA_estimate(plant_counts=test_c,plant_RMs=test_rm, precision_type="low", field_area=2)
plots <- RYA_plots(res, field_area=2)
plots[1]
thePlot <- plots[2]
thePlot
ggsave(file="Scatter3.svg", plot=thePlot, width=10, height=10)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}


points_geometryc_shape<-function(){
  x <- c(3,8,5,4)
  y <- c(3,4,5,8)
  points <- data.frame(x, y)
  return(points)
}

plot_point_poligon<-function(pointsOriginal, colourOriginal, 
                             pointsNew, colourNew,
                             title,
                             min_x = -6, max_x = 8.1, min_y = 0, max_y = 12,
                             is_symetry = F, m = NULL, n = NULL
                            ){
  n_puntos <- length(pointsOriginal$x)
  
  colourOriginal <- rep(colourOriginal, n_puntos)
  colourNew <- rep(colourNew, n_puntos)
  colour <- c(colourOriginal, colourNew)
  
  x <- c(pointsOriginal$x, pointsNew$x)
  y <- c(pointsOriginal$y, pointsNew$y)
  
  group <- rep('o', n_puntos)
  group <- c(group, rep('n', n_puntos))
  
  points <- data.frame(x, y, colour, group)

  if(is_symetry){
    p <- ggplot() +
      geom_point(data = points, aes(x = x, y = y)) +
      geom_polygon(data = points, mapping=aes(x=x, y=y, fill = colour, group=group), colour=colour) +
      geom_abline(slope =  m, intercept = n) +
      scale_fill_identity() + 
      labs(title=title) +
      xlim(min_x, max_x) +
      ylim(min_y, max_y)
  }else{
    p <- ggplot() +
      geom_point(data = points, aes(x = x, y = y)) +
      geom_polygon(data = points, mapping=aes(x=x, y=y, fill = colour, group=group), colour=colour) +
      scale_fill_identity() + 
      labs(title=title) +
      xlim(min_x, max_x) +
      ylim(min_y, max_y)
  }
  return(p)
}

translate_points <- function(points, vector){
  for(index in 1:nrow(points)){
    point <- c(points[index,]$x, points[index,]$y)
    point <- point + vector
    points[index, ] = point
  }
  return(points)
}

rotate_points <- function(points, angle){
  cos_angle <- cos(angle)
  sen_angle <- sin(angle)
  mat <- matrix(c(cos_angle, -sen_angle,
                  sen_angle, cos_angle), 
                 nrow = 2, byrow= T)
  for(index in 1:nrow(points)){
    point <- c(points[index,]$x, points[index,]$y)
    point <- mat %*% point
    points[index, ] = point
  }
  
  return(points)
}

make_axial_simetric_points <- function(points, m, n){
  t <- atan(m)
  cos_2t <- cos(2 * t)
  sen_2t <- sin(2 * t)
  mat_rot <- matrix(c(cos_2t, sen_2t,
                      sen_2t, -cos_2t), 
                    nrow = 2, byrow= T)
  mat_trans <- c(-n*sen_2t, n + n*cos_2t)
  for(index in 1:nrow(points)){
    point <- c(points[index,]$x, points[index,]$y)
    point <- mat_rot %*% point
    point <- point + mat_trans
    points[index, ] = point
  }
  
  return(points)
}

make_homothetic_points <- function(points, center, k){
  mat_trans <- c(center[1] * (1 - k), center[2] * (1 - k))
  for(index in 1:nrow(points)){
    point <- c(points[index,]$x, points[index,]$y)
    point <- point * k
    point <- point + mat_trans
    points[index, ] = point
  }
  
  return(points)
}

plot_translation <- function(vector, colourOriginal, colourNew){
  points <- points_geometryc_shape()
  translated_points <- translate_points(points, vector)
  
  plot_point_poligon(points, colourOriginal, translated_points, colourNew, title = paste("Translaccion con vector (",
                                                                                         vector[1], ",", vector[2], 
                                                                                         ")", sep = ""))
}

plot_rotation <- function(angle, colourOriginal, colourNew){
  points <- points_geometryc_shape()
  rotated_points <- rotate_points(points, angle)
  
  plot_point_poligon(points, colourOriginal, rotated_points, colourNew, title = paste("Rotacion con angulo ", round(angle, 2), " rad respecto", 
                                                                                      " al (0, 0)", sep = ""))
}

plot_simetry <- function(m, n, colourOriginal, colourNew){
  points <- points_geometryc_shape()
  simetric_points <- make_axial_simetric_points(points, m, n)
  
  plot_point_poligon(points, colourOriginal, simetric_points, colourNew, is_symetry = T, m = m, n = n, 
                     title = paste("Simetria axial respecto a la linea ",
                                   m, "x + ", n
                                   , sep = ""))
}


plot_homothecy <- function(center, k, colourOriginal, colourNew){
  points <- points_geometryc_shape()
  homothetic_points <- make_homothetic_points(points, center, k)
  
  plot_point_poligon(points, colourOriginal, homothetic_points, colourNew, min_x = 3, max_x = 30, min_y = 3, max_y = 30, 
                     title = paste("Homotecia con centro (", center[1], ", ", center[2], ") y razon ", k, sep=""))
}

main <- function(){
  usePackage("tidyverse")
  
  original_colour = "red"
  transformed_colour = "blue"
  
  vector = c(-5,4)
  print(plot_translation(vector, original_colour, transformed_colour))
  
  angle = pi / 4
  print(plot_rotation(angle, original_colour, transformed_colour))
  
  m = 4
  n = 1
  print(plot_simetry(m, n, original_colour, transformed_colour))
  
  center = c(1,1)
  k = 3
  print(plot_homothecy(center, k, original_colour, transformed_colour))
}

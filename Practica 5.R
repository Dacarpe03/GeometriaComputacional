create_points<-function(){
  points <- data.frame(
    x = c(1,1,2,2,1,1,2,2),
    y = c(1,2,2,1,1,2,2,1),
    z = c(1,1,1,1,2,2,2,2)
  )
  return(points)
}


plot_cube<-function(points, title){
  # library(plotly)
  
  min_x = floor(min(points$x))
  max_x = ceiling(max(points$x))
  x_range = max_x - min_x
  
  min_y = floor(min(points$y))
  max_y = ceiling(max(points$y))
  y_range = max_y - min_y
  
  min_z = floor(min(points$z))
  max_z = ceiling(max(points$z))
  z_range = max_z - min_z
  
  axx <- list(
    nticks = x_range + 2,
    range = c(min_x - 1,max_x + 1)
  )
  
  axy <- list(
    nticks = y_range + 2,
    range = c(min_y - 1,max_y + 1)
  )
  
  axz <- list(
    nticks = z_range + 2,
    range = c(min_z - 1,max_z + 1)
  )
  
  scene = list(
    xaxis=axx,
    yaxis=axy,
    zaxis=axz
  )
  
  fig <- plot_ly(points, x = ~x, y = ~y, z = ~z , type='mesh3d', 
                 i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
                 j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
                 k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
                 facecolor = rep(toRGB(viridisLite::inferno(6)), each = 2)
                 ) %>% layout(scene = scene, title = title)
  return(fig)
}

translate_points<-function(points, vector){
  
  for(index in 1:nrow(points)){
    point <- c(points[index,]$x, points[index,]$y, points[index,]$z)
    point <- point + c(vector[1], vector[2], vector[3])
    points[index, ] = point
  }
  return(points)
}


rotate_points<-function(points, rad_angle, axis){
  cos_angle = cos(rad_angle)
  sen_angle = sin(rad_angle)
  if(axis == "x"){
    mat=matrix(c(1, 0, 0,
                 0, cos_angle, -sen_angle,
                 0, sen_angle, cos_angle), 
            nrow = 3, byrow= T)
  }
  if(axis == "y"){
    mat=matrix(c(cos_angle, 0, sen_angle,
                 0, 1, 0,
                 -sen_angle, 0, cos_angle),
               nrow = 3, byrow= T)
  }
  if(axis == "z"){
    mat=matrix(c(cos_angle, -sen_angle, 0,
                 sen_angle, cos_angle, 0,
                 0, 0, 1), 
               nrow = 3, byrow= T)
  }
  
  for(index in 1:nrow(points)){
    point <- c(points[index,]$x, points[index,]$y, points[index,]$z)
    point <- mat %*% point
    points[index, ] = point
  }
  return(points)
}

distance_point_plane<-function(point, plane){
  A <- plane[1]
  B <- plane[2]
  C <- plane[3]
  D <- plane[4]
  
  vec_D<-c(A,B,C,D)
  vec<-c(A,B,C)
  point<-c(point,1)
  
  d = (vec_D %*% point)
  d = d / (sqrt(sum(vec*vec)))
  
  return(d[1,1])
  
}

mirror_simmetry_points<-function(points, plane){
  A <- plane[1]
  B <- plane[2]
  C <- plane[3]
  D <- plane[4]
  
  n <- c(A,B,C)
  n <- n/sqrt(sum(n*n))
  
  for(index in 1:nrow(points)){
    point <- c(points[index,]$x, points[index,]$y, points[index,]$z)
    d = distance_point_plane(point, plane)
    
    test_1 = point + 2 * d * n
    d_test_1 = distance_point_plane(test_1, plane)
    
    if(d_test_1 * d < 0){
      points[index, ] = test_1
      next
    }
    
    test_2 = point - 2 * d * n
    d_test_2 = distance_point_plane(test_2, plane)
    
    if(d_test_2 * d < 0){
      points[index, ] = test_2
      next
    }
    
  }
  
  return(points)
}

homothetic_points<-function(points, center, radius){
  mat<-matrix(c(1,0,0,0,
                center[1], radius, 0, 0,
                center[2], 0, radius, 0,
                center[3], 0, 0, radius),
              nrow = 4, byrow = T)
  
  for(index in 1:nrow(points)){
    point <- c(1, points[index,]$x, points[index,]$y, points[index,]$z)
    point <- mat %*% point
    points[index, ] = point[-1]
  }
  
  return(points)
}

create_figs<-function(){
  
  points<-create_points()
  initial_cube<-plot_cube(points, "Original Cube")
  
  translated_points<-translate_points(points,  c(-2,2,2))
  translated_cube<-plot_cube(translated_points, "Translated cube  (-2,2,2)")
  
  rotated_points<-rotate_points(points,  pi/4, "x")
  rotated_cube_x<-plot_cube(rotated_points, "Rotate  cube  pi/4 relative x axis")
  
  rotated_points<-rotate_points(points,  pi/4, "y")
  rotated_cube_y<-plot_cube(rotated_points, "Rotate  cube  pi/4 relative y axis")
  
  rotated_points<-rotate_points(points,  pi/4, "z")
  rotated_cube_z<-plot_cube(rotated_points, "Rotate  cube  pi/4 relative z axis")
  
  symetric_points<-mirror_simmetry_points(points, c(4,3,0,-1))
  symetric_cube<-plot_cube(symetric_points, "Symetric cube realative y = 4x+4y-1")
    
  homothetical_points<-homothetic_points(points, c(1,1,1), 6)
  homothetic_cube<-plot_cube(homothetical_points, "homothetic cube center (1,1,1) radius 6")
  
  return(list("original_cube" = initial_cube,
              "translated_cube" = translated_cube,
              "x_rotated_cube" = rotated_cube_x,
              "y_rotated_cube" = rotated_cube_y,
              "z_rotated_cube" = rotated_cube_z,
              "symetric_cube" = symetric_cube,
              "homothetic_cube" = homothetic_cube))
  
}



movement <- function(inds){
  
  for(i in 1:n){
    # Calculate new x location for the fish given bearing and swim distance
    new_x <- as.numeric(inds[i,"x_loc"]) + inds[i,"dist"] * cos(deg2rad(inds[i,"bearing"]))
    
    # ======= Torus setup =======
    # Wrap around if move off left edge
    if(new_x < 0){
      new_x <- new_x + 500
    }
    # Wrap around if move off right edge
    if(new_x > 500){
      new_x <- new_x - 500
    }
    # ======= Torus end =========
    
    # Find cell name in raster
    new_cell    <- cellFromXY(hsi[[ceiling(ts/60/(60/s))]],
                              cbind(new_x,
                                    inds[i,"y_loc"]))
    # Save cell name and x location
    inds[i, "cellnum"]  <- new_cell
    inds[i, "x_loc"]    <- new_x
    
    rm(new_x, new_cell)
  }
  rm(i)
  # Append results to array of individuals
  return(inds);
}
movement <- function(inds){
  # Save previous bearings
  prevbear <- inds[,"bearing"]

  # Loop through fish
  for(i in 1:n){
    
    # Attract
    if(inds[i,"nn.dist"]>=attract){
      # Calculate bearing between the two
      b <- round(bearing(inds[i, "x_loc"], 
                   inds[i,"y_loc"], 
                   inds[inds[i,"nn.idx"],"x_loc"], 
                   inds[inds[i,"nn.idx"],"y_loc"]),0)
      
      # Assign bearing
      inds[i,"bearing"] <- round(360-(90-b),0)
      
      # Find coterminal angle on unit circle
      if(as.numeric(inds[i,"bearing"])>360){
        inds[i,"bearing"] <- as.numeric(inds[i,"bearing"])-360
      }
      if(as.numeric(inds[i,"bearing"])<0){
        inds[i,"bearing"] <- as.numeric(inds[i,"bearing"])+360
      }
      
      # Set turning angle the fish needs to move
      inds[i,"turn"] <- inds[i,"bearing"] - as.numeric(prevbear[i])
      
      # Check y position
      new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*sin(deg2rad(inds[i,"bearing"]))
      
      # If moved off grid in y-dir, redraw random turning angle
      while(new_y > 500){
        inds[i,"turn"]       <- round(rtruncnorm(n=1, 
                                                 mean=0, sd=67.5,
                                                 a=-135, b=135),0)
        inds[i,"bearing"] <- as.numeric(prevbear[i]) - inds[i,"turn"]
        new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*
          sin(deg2rad(inds[i,"bearing"]))
      }
      while(new_y < 0){
        inds[i,"turn"]       <- round(rtruncnorm(n=1, 
                                                 mean=0, sd=67.5,
                                                 a=-135, b=135),0)
        inds[i,"bearing"] <- as.numeric(prevbear[i]) - inds[i,"turn"]
        new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*
          sin(deg2rad(inds[i,"bearing"]))
      }
    }
  
    # Repel
    if(inds[i,"nn.dist"]!=0 & inds[i,"nn.dist"]<=repulse){
      # Initialize coin: determines if they move right or left
      coin <- c(1, -1)
      # Flip the coin
      cf <- sample(coin,size=1)
      # List of potential turns away from too-close neighbor
      p_turn <- seq(45, 135,1)
      # Randomly select one
      i_turn <- sample(p_turn,1)
      # Calculate turning angle
      inds[i,"turn"] <- cf*i_turn
      # Calculate new bearing
      inds[i,"bearing"] <- as.numeric(prevbear[i]) +inds[i,"turn"]
      # Find coterminal angle on unit circle
      if(inds[i,"bearing"]<0){
        inds[i,"bearing"] <- inds[i,"bearing"]+360
      }
      if(inds[i,"bearing"]>360){
        inds[i,"bearing"] <- inds[i,"bearing"]-360
      }
      # Check y position
      new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*sin(deg2rad(inds[i,"bearing"]))
      
      # If moved off grid in y-dir, redraw random turning angle
      while(new_y > 500){
        inds[i,"turn"]       <- round(rtruncnorm(n=1, 
                                                 mean=0, sd=67.5,
                                                 a=-135, b=135),0)
        inds[i,"bearing"] <- as.numeric(prevbear[i]) - inds[i,"turn"]
        new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*
          sin(deg2rad(inds[i,"bearing"]))
      }
      while(new_y < 0){
        inds[i,"turn"]       <- round(rtruncnorm(n=1, 
                                                 mean=0, sd=67.5,
                                                 a=-135, b=135),0)
        inds[i,"bearing"] <- as.numeric(prevbear[i]) - inds[i,"turn"]
        new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*
          sin(deg2rad(inds[i,"bearing"]))
      }
    }
  
    # Align
    if(inds[i,"nn.dist"]>repulse & inds[i,"nn.dist"]<attract){
      # Set ID of nearest neighbor
      nnb <- inds[i,"nn.idx"]
      # Set bearing and dist of nearest neighbor
      alb <- inds[nnb,"bearing"]
      ald <- inds[nnb, "dist"]
      # Set bearing and dist of individual
      inb <- inds[i,"bearing"]
      idt <- inds[i,"dist"]
      # Vector of nearest neighbor
      alx <- ald * cos(deg2rad(alb))
      aly <- ald * sin(deg2rad(alb))
      alv <- as.matrix(c(alx, aly))
      # Vector of individual
      inx <- idt * cos(deg2rad(inb))
      iny <- idt * sin(deg2rad(inb))
      inv <- as.matrix(c(inx, iny))
      # Midpoint of vector endpoints
      midx <- ((alv[1]+inv[1])/2)
      midy <- ((alv[2]+inv[2])/2)
      rads <- atan2(midy,  midx)
      # Unusual case: midpoint is 0 because they're opposite
      if(rads==0){
        # Halfway between is 90 degrees
        rads <- pi/2
      }
      # Angle to midpoint
      halfang <- round(rads * (180/pi),0)
      if(halfang<0){
        halfang <- 360+halfang
      }
      
      # Save as new bearing
      inds[i,"bearing"] <- halfang
      
      # NOT CURRENTLY IN USE ===================================
      # Add slight error to bearing alignment
      #errorseq <- seq(-10, 10, 1)
      #errorseq <- errorseq[-11]
      #errortak <- sample(errorseq, 1)
      #inds[i,"bearing"] <- round(inds[i,"bearing] + errortak,0)
      # =========================================================
      
      
      # Find coterminal angle on unit circle
      if(as.numeric(inds[i,"bearing"])>360){
        inds[i,"bearing"] <- as.numeric(inds[i,"bearing"])-360
      }
      if(as.numeric(inds[i,"bearing"])<0){
        inds[i,"bearing"] <- as.numeric(inds[i,"bearing"])+360
      }
      # Calculate turning angle
      inds[i,"turn"] <- inds[i,"bearing"] - as.numeric(prevbear[i])
      # Find coterminal angle on unit circle
      if(inds[i,"turn"]<(-135)){
        inds[i,"turn"] <- inds[i,"turn"]+360
      }
      if(inds[i,"turn"]>135){
        inds[i,"turn"] <- inds[i,"turn"]-360
      }
      # Check y position
      new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*sin(deg2rad(inds[i,"bearing"]))
      
      # If moved off grid in y-dir, redraw random turning angle
      while(new_y > 500){
        inds[i,"turn"]       <- round(rtruncnorm(n=1, 
                                                 mean=0, sd=67.5,
                                                 a=-135, b=135),0)
        inds[i,"bearing"] <- as.numeric(prevbear[i]) - inds[i,"turn"]
        new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*
          sin(deg2rad(inds[i,"bearing"]))
      }
      while(new_y < 0){
        inds[i,"turn"]       <- round(rtruncnorm(n=1, 
                                                 mean=0, sd=67.5,
                                                 a=-135, b=135),0)
        inds[i,"bearing"] <- as.numeric(prevbear[i]) - inds[i,"turn"]
        new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*
          sin(deg2rad(inds[i,"bearing"]))
      }
    }
  
  # First order random walk if no nearest neighbor
  # Loop through fish
    # If no nearest neighbor
    if(inds[i,"nn.dist"]==0){
      # Random turn within vision arc
      inds[i,"turn"]       <- as.numeric(sample(seq(-45, 45, by=1), 1))
      # Calculate new bearing
      inds[i,"bearing"] <- inds[i,"turn"] + as.numeric(prevbear[i])
      # Find coterminal angle on unit circle
      if(inds[i,"bearing"]<0){
        inds[i,"bearing"] <- inds[i,"bearing"]+360
      }
      if(inds[i,"bearing"]>360){
        inds[i,"bearing"] <- inds[i,"bearing"]-360
      }
      # Check y position
      new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*sin(deg2rad(inds[i,"bearing"]))
      
      # If moved off grid in y-dir, redraw random turning angle
      while(new_y > 500){
        inds[i,"turn"]       <- round(rtruncnorm(n=1, 
                                                 mean=0, sd=67.5,
                                                 a=-135, b=135),0)
        inds[i,"bearing"] <- as.numeric(prevbear[i]) - inds[i,"turn"]
        new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*
          sin(deg2rad(inds[i,"bearing"]))
      }
      while(new_y < 0){
        inds[i,"turn"]       <- round(rtruncnorm(n=1, 
                                                 mean=0, sd=67.5,
                                                 a=-135, b=135),0)
        inds[i,"bearing"] <- as.numeric(prevbear[i]) - inds[i,"turn"]
        new_y <- as.numeric(inds[i,"y_loc"]) + inds[i,"dist"]*
          sin(deg2rad(inds[i,"bearing"]))
      }
    }
  }
  
  # Calculate new xy locations for the fish
  inds[,"x_loc"] <- inds[, "x_loc"] +
    inds[,"dist"]*cos(deg2rad(inds[,"bearing"]))
  inds[,"y_loc"] <- inds[, "y_loc"] +
    inds[,"dist"]*sin(deg2rad(inds[,"bearing"]))
  
  # ======= Torus setup =======
  for(i in 1:n){
    # Wrap around if move off left edge
    if(inds[i,"x_loc"]<0){
      inds[i,"x_loc"] <- inds[i,"x_loc"]+100
    }
    # Wrap around if move off right edge
    if(inds[i,"x_loc"]>100){
      inds[i,"x_loc"] <- inds[i,"x_loc"]-100
    }
  }
  # ======= Torus end
  # Append results to array of individuals
  return(inds);
}

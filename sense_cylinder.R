sense <- function(inds){
  # Loop through individuals
  for(i in 1:n){
    # Calculate physical distance to move
    dist <- as.numeric(inds[i,"body_len"]) * (as.numeric(inds[i,"swimspd"]) * s)
    
    # Create vectors to store info for adjacent cells looking right
    b_x_loc <- vector(mode="numeric", length=length(turnseq))
    b_y_loc <- vector(mode="numeric", length=length(turnseq))
    b_cellnum <- vector(mode="numeric", length=length(turnseq))
    
    # Loop through all possible turning angles
    for(k in 1:length(turnseq)){
      # Sense looking right, x location
      b_x_loc[k] <- inds[i,"x_loc"] + inds[i,"sensedist"] *
        cos(deg2rad(inds[i,"bearing"] - turnseq[k]))
      # Sense looking right, y location
      b_y_loc[k] <- inds[i,"y_loc"] + inds[i,"sensedist"] *
        sin(deg2rad(inds[i,"bearing"] - turnseq[k]))
      
      # If near the x limits of the grid, sense around torus shape
      # Sense below 0 in x direction
      if(b_x_loc[k]<0){
        b_x_loc[k] <- b_x_loc[k]+500
      }
      
      # Sense above x limit (500)
      if(b_x_loc[k]>500){
        b_x_loc[k] <- b_x_loc[k]-500
      }
    }
    
    b_cells <- cbind(b_x_loc, b_y_loc, turnseq)
    b_cells <- subset(b_cells, b_cells[,"b_y_loc"] > 0 &
                        b_cells[,"b_y_loc"] < 2500)
    
    # Pull cell numbers from xy location of sensing radius
    b_cellnum <- cellFromXY(estov,
                            cbind(b_cells[,"b_x_loc"],
                                  b_cells[,"b_y_loc"]))
    
    # Pull HSI values for "sensed" cells
    scorenums <- estov[b_cellnum]
    
    b_cells <- cbind(b_cells, b_cellnum, scorenums)
    
    # Combine cellnums and bearings
    pick <- unique(b_cells)
    pick <- na.omit(pick)
    
    # Sense cell fish is currently in
    homecell <- as.numeric(inds[i,"cellnum"])
    homescore <- as.numeric(inds[i,"score"])
    
    cellnums <- unique(pick[,"b_cellnum"])
    scorenums <- estov[cellnums]
    
    # Combine sensed cells and home cell in a single array
    allcells <- c(cellnums, homecell)
    allscores <- c(scorenums, homescore)
    cells <- cbind(allcells, allscores)
    cells <- cells[order(cells[,2], decreasing=T),]
    cells <- unique(cells)
    
    # Are any cells more than 0.2 different from another sensed cell?
    x <- cells[,"allscores"]
    y <- sort(x)
    g <- cumsum(c(1, abs(y[-length(y)] - y[-1]) > notice))
    groups <- by(y, g, identity)
    
    # Determine the cell number of the "best" nearby cell by HSI
    # If all cells are within 0.2 of each other, it picks a random one
    best <- cells[(cells[,2])==sample(groups[[length(groups)]],1)][1]
    
    # If current cell is better than surrounding cells, go towards random cell
    # First order random walk
    if(best==homecell){
      posang <- seq(-45, 45, by=1)
      if(length(posang)>1){
        ang <- sample(posang, 1)
        bearing <- as.numeric(inds[i,"bearing"]) - ang
        new_y <- as.numeric(inds[i,"y_loc"]) + dist*sin(deg2rad(bearing))
        while(new_y > 2500){
          biggerrange <- seq(-135, 135, by=1)
          biggerrange <- biggerrange[! biggerrange %in% ang]
          ang <- sample(biggerrange, 1)
          bearing <- as.numeric(inds[i,"bearing"]) - ang
          new_y <- as.numeric(inds[i,"y_loc"]) + dist*sin(deg2rad(bearing))
        }
        while(new_y < 0){
          biggerrange <- seq(-135, 135, by=1)
          biggerrange <- biggerrange[! biggerrange %in% ang]
          ang <- sample(biggerrange, 1)
          bearing <- as.numeric(inds[i,"bearing"]) - ang
          new_y <- as.numeric(inds[i,"y_loc"]) + dist*sin(deg2rad(bearing))
        }
      }
      if(length(posang)==1){
        ang <- posang
        bearing <- as.numeric(inds[i,"bearing"]) - ang
        new_y <- as.numeric(inds[i,"y_loc"]) + dist*sin(deg2rad(bearing))
      }
    }
    
    # If another cell is best, pick random bearing towards best cell
    if(best!=homecell){
      minang <- min(pick[,"turnseq"][pick[,"b_cellnum"]==best])
      maxang <- max(pick[,"turnseq"][pick[,"b_cellnum"]==best])
      posang <- seq(minang, maxang, by=1)
      if(length(posang)>1){
        ang <- sample(posang, 1)
        ang <- sample(posang, 1)
        bearing <- as.numeric(inds[i,"bearing"]) - ang
        new_y <- as.numeric(inds[i,"y_loc"]) + dist*sin(deg2rad(bearing))
        while(new_y > 2500){
          biggerrange <- seq(-135, 135, by=1)
          biggerrange <- biggerrange[! biggerrange %in% ang]
          ang <- sample(biggerrange, 1)
          bearing <- as.numeric(inds[i,"bearing"]) - ang
          new_y <- as.numeric(inds[i,"y_loc"]) + dist*sin(deg2rad(bearing))
        }
        while(new_y < 0){
          biggerrange <- seq(-135, 135, by=1)
          biggerrange <- biggerrange[! biggerrange %in% ang]
          ang <- sample(biggerrange, 1)
          bearing <- as.numeric(inds[i,"bearing"]) - ang
          new_y <- as.numeric(inds[i,"y_loc"]) + dist*sin(deg2rad(bearing))
        }
      }
      if(length(posang)==1){
        ang <- posang
        bearing <- as.numeric(inds[i,"bearing"]) - ang
        new_y <- as.numeric(inds[i,"y_loc"]) + dist*sin(deg2rad(bearing))
      }
      rm(minang, maxang)
    }
    
    # Calculate new bearing given turning angle from sense function
    bearing <- as.numeric(inds[i,"bearing"]) - ang
    
    if(bearing > 360){
      bearing <- bearing-360
    }
    if(bearing < 0){
      bearing <- bearing + 360
    }
    
    inds[i, "bearing"]  <- bearing
    inds[i, "turn"]     <- ang
    inds[i, "dist"]     <- dist
    inds[i, "y_loc"]    <- new_y
    
    rm(cells, pick, allcells, allscores, b_cellnum, b_x_loc, b_y_loc, best,
       cellnums, homecell, homescore, k, posang, scorenums, b_cells, x, y, g,
       groups, new_y, ang, bearing, dist)
  }
  rm(i)
  # Append results to array of individuals
  return(inds)
}
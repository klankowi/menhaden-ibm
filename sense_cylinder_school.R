sense <- function(inds){
  # Create matrix of fish positions
  inds.mat <- cbind(inds[,"x_loc"], inds[,"y_loc"],
                        inds[,"individual"])
  names(inds.mat) <- c('x', 'y', 'ID')
  
  # Elements for joined matrix
  x.a <- rep(inds[1,"x_loc"],100)
  for(i in 2:100){
    additive <- rep(inds[i,"x_loc"],100)
    x.a <- c(x.a, additive)
  }
  rm(i)
  
  y.a <- rep(inds[1,"y_loc"],100)
  for(i in 2:100){
    additive <- rep(inds[i,"y_loc"],100)
    y.a <- c(y.a, additive)
  }
  rm(i)
  
  x.b <- rep(inds[,"x_loc"],100)
  y.b <- rep(inds[,"y_loc"],100)
  
  xdistto <- x.a - x.b
  ydistto <- y.a - y.b
  distto  <- sqrt((x.a - x.b)^2 + (y.a - y.b)^2)
  
  # Matrix of all possible pointwise distances
  m <- cbind(ID.a, x.a, y.a, ID.b, x.b, y.b, xdistto, ydistto, distto)
  m <- m[m[,"ID.a"] != m[,"ID.b"],]
  
  # Delete pointwise pairs further away than sensedist
  m <- m[abs(m[,"xdistto"]) < max(inds[,"sensedist"]),]
  m <- m[abs(m[,"ydistto"]) < max(inds[,"sensedist"]),]
  
  # Use nnt (donut pkg) and knn (nabor pkg) to find nn within sensedist
  # Loop through all fish
  for(i in 1:n){
    # Call neighbors potentially within fish's sensedist
    neighbs <- m[m[,"ID.a"] == i,]
    
    # If only one possible neighbor, have to coerce to matrix
    if(length(neighbs)==9){
      neighbs <- t(as.matrix(neighbs, nrow=1))
    }
    
    # If no nearby fish, move to next individual
    if(nrow(neighbs)==0){
      next()
    }
    
    # Set number of neighbors to those possibly within sensedist
    # Add one, because closest neighbor is technically itself
    k=nrow(neighbs)+1
    
    # Call position of fish
    query <- data.frame(inds[i,"x_loc"], inds[i,"y_loc"])
    
    # Find ID of and distance to nearest fish
    res2 <- nnt(inds[,1:2], 
                query, k=k, fn=nabor::knn, torus=1, ranges=ranges,
                radius=inds[i,"sensedist"])
    
    # Store IDs
    nnb <- unlist(res2$nn.idx[2:length(res2$nn.idx)])
    nnb <- nnb[nnb!=0]
    
    # Store distances
    nnd <- unlist(res2$nn.dists[2:length(res2$nn.idx)])
    nnd <- nnd[is.infinite(nnd)==FALSE]
    
    # Combine into matrix, order
    nndf <- cbind(nnb, nnd)
    if(nrow(nndf) > 1){
     nndf <- nndf[order(nndf[,"nnd"], decreasing = T),] 
    }
    
    # If no nearby fish, move to next individual
    if(nrow(nndf)==0){
      next()
    }
    
    # Cycle through nearest neighbors to find closest in vision arc
    for(z in 1:nrow(nndf)){
      # Calculate bearing to neighbor z
      b <- bearing(inds[i,"x_loc"],
                   inds[i,"y_loc"],
                   inds[as.numeric(nndf[z,"nnb"]),"x_loc"],
                   inds[as.numeric(nndf[z,"nnb"]),"y_loc"])
      # Calculate angular difference to neighbor z
      ang <- round(360-(90-b),0)
      # Find coterminal angle in unit circle
      if(as.numeric(ang)>360){
        ang <- as.numeric(ang)-360
      }
      # Maximum distance fish can see to left and right
      sl <- as.numeric(inds[i,"bearing"])+135
      sr <- as.numeric(inds[i,"bearing"])-135
      
      # All possible bearings the fish can turn
      sll <- seq(as.numeric(inds[i,"bearing"]),sl, by=1)
      srr <- seq(sr, as.numeric(inds[i,"bearing"]), by=1)
      
      # Find coterminal angles in unit circle
      for(q in 1:length(sll)){
        if(sll[q]>360){
          sll[q] <- sll[q]-360
        }
      }
      for(j in 1:length(srr)){
        if(srr[j]<0){
          srr[j] <- srr[j]+360
        }
      }
      
      # Create vector of "good" angles that coincide with vision arc
      goodangs <- c(sll, srr)
      goodangs <- goodangs[order(goodangs)]
      goodangs <- unique(goodangs)
      
      # If angle to attract/ align with nearest neighbor is within arc
      if(ang %in% goodangs){
        # Nearest neighbor ID copied
        inds[i,"nn.idx"] <- as.numeric(nndf[z,"nnb"])
        # Distance to nearest neighbor copied
        inds[i,"nn.dist"] <- nnd[z]
        # Angle to attract/ align copied
        inds[i,"ang"] <- ang
        # Flag set to 1
        inds[i,"tru"] <- 1
      }
      
      # If angle to attract/ align with nearest neighbor isn't within arc
      if(ang %in% goodangs == FALSE){
        # Flag remains at 0
        inds[i,"tru"] <- 0
      }
      
      # If there are no neighbors within vision arc
      if(ang %in% goodangs == FALSE & z==length(nnb)){
        # Move to next loop iteration
        next()
      }
    }
    rm(neighbs, nndf, query, res2, ang, b, goodangs, 
       k, nnb, nnd, sl, sll, sr, srr, z, q, j)
  }
  rm(inds.mat, m, distto, i, x.a, x.b, xdistto, y.a, y.b, ydistto, additive)
  # Append results to array of individuals
  return(inds)
}
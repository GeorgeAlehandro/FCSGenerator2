#Modify the FCSG2 objects : move population, change size, supress or add pop

source("FCSGframework.R")

move.population <- function(ref.object, pop.id = 1, marker.id = 1, new.position = c(Inf,Inf), min.val=0, max.val=4.5,
                            marker.columns)
{
    tmp.object <- ref.object
    mat <- tmp.object$Expr[[pop.id]]
    #WARNING : THE FOLLOWING CODE SHOULD BE LOOKED INTO MORE CAREFULY
    #marker.id in positions might be different than column id
    old.position <- c(tmp.object$Positions[[pop.id]][[1]][[which(sort(marker.columns)==marker.id)[[1]]]], 
                      tmp.object$Positions[[pop.id]][[2]][[which(sort(marker.columns)==marker.id)[[1]]]])
    if(new.position[1]==Inf)
    {
        new.position[1] <- old.position[1]
    }
    if(new.position[2]==Inf)
    {
        new.position[2] <- old.position[2]
    }
    
    mat[,marker.id] <- rtruncnorm(nrow(mat), min.val, max.val, new.position[1], new.position[2])

    tmp.object$Positions[[pop.id]][[1]][[marker.id]] <- new.position[1]
    tmp.object$Positions[[pop.id]][[2]][[marker.id]] <- new.position[2]
    tmp.object$Expr[[pop.id]] <- mat
    return(tmp.object)
}

change.population.size <- function(ref.object, pop.id = 1, new.size = Inf, min.val=0, max.val=4.5,
                                   annotation.column, marker.columns)
{
    tmp.object <- ref.object
    mat <- tmp.object$Expr[[pop.id]]
    #WARNING : THE FOLLOWING CODE SHOULD BE LOOKED INTO MORE CAREFULY
    #marker.id in positions might be different than column id
    old.size <- tmp.object$Frequencies[[pop.id]]
    
    if(new.size==Inf)
    {
        new.size <- old.size
    }
    if(new.size < 100)
    {
        r <- 100 - new.size
        nmb.events <- nrow(mat)
        nmb.removed.events <- as.integer(r * nmb.events / 100)
        if( (nmb.removed.events > 0) && (nmb.removed.events < (nmb.events-1)) ) #a population with just 1 event might create some issues with matrices
        {
            mat <- mat[sample(1:nmb.events, nmb.events - nmb.removed.events),]
        }
    }
    else if(new.size >= 100)
    {
        r <- new.size - 100
        nmb.events <- nrow(mat)
        nmb.new.events <- as.integer(r * nmb.events / 100)
        added.mat <- matrix(1, nrow=nmb.new.events, ncol=ncol(mat))
        for(k in 1:length(marker.columns))
        {
            old.position <- c(tmp.object$Positions[[pop.id]][[1]][[k]], 
                              tmp.object$Positions[[pop.id]][[2]][[k]])
            marker <- marker.columns[[k]]
            added.mat[,marker.columns[[k]]] <- rtruncnorm(nmb.new.events, min.val, max.val, old.position[1], old.position[2])
            added.mat[,annotation.column] <- mat[1, annotation.column]
        }
        mat <- rbind(mat, added.mat)
    }
    
    tmp.object$Expr[[pop.id]] <- mat
    
    total.events <- sum(sapply(1:length(tmp.object$Expr), function(i)
    {
        return(nrow(tmp.object$Expr[[i]]))
    }))
    for(i in 1:length(tmp.object$Expr))
    {
        tmp.object$Frequencies[[i]] <- nrow(tmp.object$Expr[[i]]) / total.events * 100
    }
    
    return(tmp.object)
}

delete.population <- function(ref.object, pop.id = 1)
{
    tmp.object <- ref.object
    tmp.object$Frequencies <- lapply(c(1:length(ref.object$Expr))[-pop.id], function(i)
    {
        return(ref.object$Frequencies[[i]])
    })
    tmp.object$Patterns <- lapply(c(1:length(ref.object$Expr))[-pop.id], function(i)
    {
        return(ref.object$Patterns[[i]])
    })
    tmp.object$Positions <- lapply(c(1:length(ref.object$Expr))[-pop.id], function(i)
    {
        return(ref.object$Positions[[i]])
    })
    tmp.object$Expr <- lapply(c(1:length(tmp.object$Expr))[-pop.id], function(i)
    {
        return(ref.object$Expr[[i]])
    })
    
    total.events <- sum(sapply(1:length(tmp.object$Expr), function(i)
    {
        return(nrow(tmp.object$Expr[[i]]))
    }))
    for(i in 1:length(tmp.object$Expr))
    {
        tmp.object$Frequencies[[i]] <- nrow(tmp.object$Expr[[i]]) / total.events * 100
    }
    
    return(tmp.object)
}

add.population <- function(ref.object, nmb.events = 10,
                           annotation.column, marker.columns, pattern = NULL, position = NULL, ref.pop = 1,
                           min.val=0, max.val=4.5)
{
    tmp.object <- ref.object
    #WARNING : THE FOLLOWING CODE SHOULD BE LOOKED INTO MORE CAREFULY
    #marker.id in positions might be different than column id
    if(!is.null(pattern))
    {
        position <- generate.position.from.pattern(pattern, min.val, max.val, 2)
    }
    else
    {
        if(is.null(position))
        {
            position <- tmp.object$Positions[[ref.pop]]
        }
    }
    
    pop.id <- length(tmp.object$Expr) + 1
    
    mat <- generate.expression.matrix.from.position(pop.id, position, nmb.events, min.val, max.val)
    
    tmp.object$Expr[[pop.id]] <- mat
    tmp.object$Positions[[pop.id]] <- position
    tmp.object$Patterns[[pop.id]] <- rep(0, length(marker.columns))
    total.events <- sum(sapply(1:length(tmp.object$Expr), function(i)
    {
        return(nrow(tmp.object$Expr[[i]]))
    }))
    for(i in 1:length(tmp.object$Expr))
    {
        tmp.object$Frequencies[[i]] <- nrow(tmp.object$Expr[[i]]) / total.events * 100
    }
    
    return(tmp.object)
}
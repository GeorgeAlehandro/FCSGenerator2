source("FCSGframework.R")
library(msm)


#GENERATE a Single FCS
generate.FCSG2 <- function(nmb.events, nmb.dim, nmb.populations, min.freq.list = NULL, max.freq.list = NULL, min.val=0, max.val=4.5)
# nmb.events  = number of events
# nmb.dim = number of markers / parameters / dimensions
# nmb.populations = number of clusters / populations
# min.freq.list = min percentage of events of each population (relative to nmb.events). If NULL, all populations have the same size
# max.freq.list = max percentage of events of each population (relative to nmb.events)
# min.val = lower boundary of the parameters's values
# max.val = upper boundary of the parameters's values
{
    fcs <- NULL
    if(is.null(min.freq.list))
    {
        min.freq.list <- rep(nmb.events/nmb.populations*100, nmb.populations)
    }
    else
    {
        if(is.null(max.freq.list))
        {
            max.freq.list <- min.freq.list + (100 - sum(min.freq.list))/nmb.populations + 1
        }
    }
    if(!(sum(min.freq.list)<=100 && sum(max.freq.list)>=100))
    {
        stop()
    }
    
    #0 - Adjust the frequencies of the populations to make the sum reach 100%
    #======
    pop.frequencies <- min.freq.list
    freq.rep.order <- rev(order(max.freq.list-pop.frequencies))
    nmb.free.pop <- nmb.populations
    #==POP FREQUENCIES ADJUSTED HERE
    while((100-sum(pop.frequencies))>1/10000)
    {
        remaining.percentage <- trunc(10000*(100-sum(pop.frequencies)))/10000
        diff.freq <- max.freq.list-pop.frequencies
        
        for(i in 1:nmb.populations)
        {
            if(pop.frequencies[freq.rep.order[i]] < max.freq.list[[freq.rep.order[i]]] &&
               remaining.percentage>0)
            {
                attributed.percentage <- min(remaining.percentage/nmb.free.pop, 
                                             diff.freq[freq.rep.order[i]])
                
                pop.frequencies[freq.rep.order[i]] <- pop.frequencies[freq.rep.order[i]] + 
                    attributed.percentage
                
                remaining.percentage <- remaining.percentage - attributed.percentage
                diff.freq <- max.freq.list-pop.frequencies
            }
        }
    }
    #======
    # browser()
    
    
    #1 - Generate a pattern for each population: pos / neg position of each population for each marker
    #eg NK : 12122 ==> neg on markers 1 and 3, pos on markers 2, 4 and 5
    #this is done using another function part of the framework
    #======
    pop.patterns.list <- generate.pattern.codes(nmb.populations, nmb.dim)
    #======
    
    #2 - Generate a list of means and SDs for each population by using the pattern list
    #this is done using another function part of the framework
    #the output is a list of 2 lists: the means list and the SDs list for each population
    #======
    pop.position.list <- lapply(1:nmb.populations, function(i)
    {
        return(generate.position.from.pattern(pop.patterns.list[[i]], min.val, max.val, 2))
    })
    #======
    
    
    #3 - Generate an expression matrix for each population
    #this is done using another function part of the framework
    #the output is a list of 2 lists: the means list and the SDs list
    #======
    event.id <- 0
    file.mat <- lapply(1:nmb.populations, function(current.pop)
    {
        pop.events <- max(1,as.integer(pop.frequencies[current.pop]*nmb.events/100))
        event.id <<- event.id+pop.events
        if(event.id<nmb.events && current.pop==nmb.populations)
        {
            pop.events <- pop.events+(nmb.events-event.id)
        }
        exp.mat <- generate.expression.matrix.from.position(current.pop, pop.position.list[[current.pop]],
                                                            pop.events,lower.value = min.val, upper.value = max.val)
        
        return(exp.mat)
    })
    #======
    
    
    #4 - Generate the pipeline object
    #a set of 4 lists
    #   the frequencies list
    #   the expression matrices list
    #   the positions list [(mean,sd), ...]
    #   the patterns list [(01101001), ...]
    #======
    fcs.object <- list("Frequencies" = pop.frequencies,
                       "Expr" = file.mat,
                       "Positions" = pop.position.list,
                       "Patterns" = pop.patterns.list)
    
    return(fcs.object)
}


#Changing the position from pos to neg (or neg to pos) only works with generated references
generate.FCSG2.files.from.reference <- function(ref.object, nmb.files, 
                                              markers.to.change = NULL, markers.to.invert = NULL,
                                              m.shift = 0.2, sd.shift = 0.8,
                                              min.val=0, max.val=4.5)
{
    markers.to.change <- as.integer(unlist(markers.to.change))
    markers.to.invert <- as.integer(unlist(markers.to.invert))
    file.objects.list <- lapply(1:nmb.files, function(i)
    {
        tmp.obj <- ref.object
        for(j in 1:length(tmp.obj$Expr))
        {
            mat <- tmp.obj$Expr[[j]]
            if( !is.null( unique(c(unlist(markers.to.change),unlist(markers.to.invert)) )) )
            {
                for( k in unique(c(unlist(markers.to.change),unlist(markers.to.invert))) )
                {
                    m <- tmp.obj$Positions[[j]][[1]][[k]] + runif(1, 0, m.shift)
                    s <- tmp.obj$Positions[[j]][[2]][[k]] + runif(1, 0, sd.shift)
                    tmp.obj$Positions[[j]][[1]][[k]] <- m
                    tmp.obj$Positions[[j]][[2]][[k]] <- s
                    if(k %in% markers.to.invert)
                    {
                        if(tmp.obj$Patterns[[j]][[k]] %in% c(1,2))
                        {
                            tmp.obj$Patterns[[j]][[k]] <- 1 + (tmp.obj$Patterns[[j]][[k]] %% 2)
                            pattern <- tmp.obj$Patterns[[j]][[k]]
                            
                            l <- length(pattern)
                            range.unit <- (max.val - min.val)/2
                            
                            m <- pattern
                            m <- runif(1, (m-2/3)*range.unit, (m-1/2)*range.unit) + runif(1, 0, m.shift)
                            s <- range.unit/l/3 + runif(1, 0, sd.shift)
                            
                            mat[,k] <- rtnorm(nrow(mat), m, s, min.val, max.val)
                        }
                    }
                    else
                    {
                        mat[,k] <- rtnorm(nrow(mat), m, s, min.val, max.val)
                    }
                }
            }
            tmp.obj$Expr[[j]] <- mat
        }
        
        return(tmp.obj)
    })
}



generate.FCSG2.file.by.reducing.populations.in.ref <- function(ref.object, 
                                                             pop.reduction.percentages, locked.populations = NULL,
                                                             marker.columns, annotation.column,
                                                             min.val=0, max.val=4.5)
{
    tmp.object <- ref.object
    free.populations <- 1:length(tmp.object$Expr)
    free.populations <- free.populations[!(free.populations%in%locked.populations)]
    free.populations <- free.populations[free.populations%in%which(pop.reduction.percentages==0)]
    
    
    for(i in 1:length(tmp.object$Expr))
    {
        r <- pop.reduction.percentages[[i]]
        nmb.events <- nrow(tmp.object$Expr[[i]])
        nmb.removed.events <- as.integer(r * nrow(tmp.object$Expr[[i]]) / 100)
        if( (nmb.removed.events > 0) && (nmb.removed.events < (nmb.events-1)) ) #a population with just 1 event might create some issues with matrices
        {
            tmp.object$Expr[[i]] <- tmp.object$Expr[[i]][sample(1:nmb.events, nmb.events - nmb.removed.events),]
            for(j in free.populations)
            {
                if(!j==i)
                {
                    nmb.new.events <- as.integer(nmb.removed.events/length(free.populations))
                    added.mat <- matrix(1, nrow=nmb.new.events, ncol=ncol(tmp.object$Expr[[i]]))
                    for(k in 1:length(marker.columns))
                    {
                        marker <- marker.columns[[k]]
                        m <- tmp.object$Positions[[j]][[1]][[k]]
                        s <- tmp.object$Positions[[j]][[2]][[k]]
                        added.mat[,marker.columns[[k]]] <- rtnorm(nmb.new.events, m, s, min.val, max.val)
                        added.mat[,annotation.column] <- tmp.object$Expr[[j]][1, annotation.column]
                    }
                    tmp.object$Expr[[j]] <- rbind(tmp.object$Expr[[j]], added.mat)
                }
            }
        }
    }
    
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
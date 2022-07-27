#Merge populations from 2 files into new populations

source("FCSGframework.R")

merge.files.populations <- function(ref.obj1, ref.obj2, pop.ids.from.ref1, pop.ids.from.ref2, 
                        new.pop.ids.from.ref1 = NULL, new.pop.ids.from.ref2 = NULL, 
                        annotation.column.1, annotation.column.2, markers.1, markers.2)
{
    pop.ids.1 <- new.pop.ids.from.ref1
    if(is.null(new.pop.ids.from.ref1))
    {
        pop.ids.1 <- sapply(1:length(ref.obj1$Expr), function(i)
        {
            return(ref.obj1$Expr[[i]][1,annotation.column.1])
        })
    }
    pop.ids.2 <- new.pop.ids.from.ref2
    if(is.null(new.pop.ids.from.ref1))
    {
        pop.ids.2 <- sapply(1:length(ref.obj2$Expr), function(i)
        {
            return(ref.obj2$Expr[[i]][1,annotation.column.2])
        })
    }
    same.ids <- intersect(pop.ids.1, pop.ids.2)
    
    all.pop.ids <- sort(c(same.ids, pop.ids.1[!pop.ids.1%in%same.ids], pop.ids.2[!pop.ids.2%in%same.ids]))
    
    mat.list <- list()
    position.list <- list()
    pattern.list <- list()
    frequencies.list <- list()
    for(i in all.pop.ids)
    {
        id.1 <- which(new.pop.ids.from.ref1==i)
        mat.1 <- NULL
        if(length(id.1)>0)
        {
            mat.1 <- ref.obj1$Expr[[ pop.ids.from.ref1[[id.1[[1]]]] ]][,markers.1]
        }
        
        id.2 <- which(new.pop.ids.from.ref2==i)
        mat.2 <- NULL
        if(length(id.2)>0)
        {
            mat.2 <- ref.obj2$Expr[[ pop.ids.from.ref2[[id.2[[1]]]] ]][,markers.2]
        }
        mat.list[[i]] <- NULL
        if(!is.null(mat.1))
        {
            mat.list[[i]] <- mat.1
        }
        if(!is.null(mat.2))
        {
            if(length(mat.list)<i)
            {
                mat.list[[i]] <- mat.2
            }
            else
            {
                mat.list[[i]] <- rbind(mat.list[[i]], mat.2)
            }
        }
        
        position.list[[i]] <- list()
        position.list[[i]][[1]] <- sapply(1:ncol(mat.list[[i]]), function(j)
        {
            return( mean( mat.list[[i]][,j] ) )
        })
        position.list[[i]][[2]] <- sapply(1:ncol(mat.list[[i]]), function(j)
        {
            s <- sd( mat.list[[i]][,j] ) 
            s <- ifelse(is.na(s), 0, s)
            
            return(s)
        })
        pattern.list[[i]] <- rep(0, ncol(mat.list[[i]]))
        mat.list[[i]] <- cbind(mat.list[[i]], "annotationID"=i)
    }
    
    total.events <- sum(sapply(1:length(mat.list), function(i)
    {
        return(nrow(mat.list[[i]]))
    }))
    for(i in 1:length(mat.list))
    {
        frequencies.list[[i]] <- nrow(mat.list[[i]]) / total.events * 100
    }
    
    
    fcs.object <- list("Frequencies" = frequencies.list,
                       "Expr" = mat.list,
                       "Positions" = position.list,
                       "Patterns" = pattern.list)
    
    return(fcs.object)
}

mix.files <- function(ref.obj1, ref.obj2, ref1.coef, ref2.coef,
                      annotation.column.1, annotation.column.2, markers.1, markers.2)
{
    pop.ids.1 <- sapply(1:length(ref.obj1$Expr), function(i)
    {
        return(ref.obj1$Expr[[i]][1,annotation.column.1])
    })
    pop.ids.2 <- sapply(1:length(ref.obj2$Expr), function(i)
    {
        return(ref.obj2$Expr[[i]][1,annotation.column.2])
    })
    same.ids <- intersect(pop.ids.1, pop.ids.2)
    
    all.pop.ids <- sort(c(same.ids, pop.ids.1[!pop.ids.1%in%same.ids], pop.ids.2[!pop.ids.2%in%same.ids]))
    
    mat.list <- list()
    position.list <- list()
    pattern.list <- list()
    frequencies.list <- list()
    t1 <- ref1.coef
    t2 <- ref2.coef
    for(i in all.pop.ids)
    {
        id.1 <- which(pop.ids.1==i)
        mat.1 <- NULL
        if(length(id.1)>0)
        {
            mat.1 <- ref.obj1$Expr[[ id.1 ]][,markers.1]
            mat.1 <- mat.1[sample(1:nrow(mat.1), as.integer(t1*nrow(mat.1)/100)), ]
        }
        
        id.2 <- which(pop.ids.2==i)
        mat.2 <- NULL
        if(length(id.2)>0)
        {
            mat.2 <- ref.obj2$Expr[[ id.2 ]][,markers.2]
            mat.2 <- mat.2[sample(1:nrow(mat.2), as.integer(t2*nrow(mat.2)/100)), ]
        }
        mat.list[[i]] <- NULL
        if(!is.null(mat.1))
        {
            mat.list[[i]] <- mat.1
        }
        if(!is.null(mat.2))
        {
            if(length(mat.list)<i)
            {
                mat.list[[i]] <- mat.2
            }
            else
            {
                mat.list[[i]] <- rbind(mat.list[[i]], mat.2)
            }
        }
        
        position.list[[i]] <- list()
        position.list[[i]][[1]] <- sapply(1:ncol(mat.list[[i]]), function(j)
        {
            return( mean( mat.list[[i]][,j] ) )
        })
        position.list[[i]][[2]] <- sapply(1:ncol(mat.list[[i]]), function(j)
        {
            s <- sd( mat.list[[i]][,j] ) 
            s <- ifelse(is.na(s), 0, s)
            
            return(s)
        })
        pattern.list[[i]] <- rep(0, ncol(mat.list[[i]]))
        mat.list[[i]] <- cbind(mat.list[[i]], "annotationID"=i)
    }
    
    total.events <- sum(sapply(1:length(mat.list), function(i)
    {
        return(nrow(mat.list[[i]]))
    }))
    for(i in 1:length(mat.list))
    {
        frequencies.list[[i]] <- nrow(mat.list[[i]]) / total.events * 100
    }
    
    
    fcs.object <- list("Frequencies" = frequencies.list,
                       "Expr" = mat.list,
                       "Positions" = position.list,
                       "Patterns" = pattern.list)
    
    return(fcs.object)
}

#CONVERSION BETWEEN THE OBJECT USED IN THE PIPELINE AND THE FCS FILE FORMAT

source("FCSGframework.R")

save.object.as.FCS <- function(fcsg2.object) #return an FCS
{
    exp.mat <- NULL
    for(i in 1:length(fcsg2.object[["Expr"]]))
    {
        if(is.null(exp.mat))
        {
            exp.mat <- fcsg2.object[["Expr"]][[i]]
        }
        else
        {
            exp.mat <- rbind(exp.mat, fcsg2.object[["Expr"]][[i]])
        }
    }
    
    nmb.events <- nrow(exp.mat)
    nmb.dim <- ncol(exp.mat) - 2
    
    
    fcs <- flowFrame(exp.mat)
    descR <- description(fcs)
    lapply(c(1:dim(exp.mat)[2]),function(x)
    {
        descR[[paste0("$P",x,"R")]] <<- 262144
    })
    
    nmb.grp <- min(nmb.events,1000)
    descR[["TIMESTEP"]] <- 1/nmb.grp
    for(e in 1:nmb.events)
    {
        exp.mat[e,nmb.dim+1] <- 1/nmb.grp *  as.integer(e / (nmb.events/nmb.grp))
    }
    fcs <- flowFrame(exp.mat, description = descR)
    
    return(fcs)
}

load.annotated.FCS.as.object <- function(fcs, annotation.column, markers.list) #return an FCS Generator 2 object
{
    fcs.mat <- fcs@exprs
    markers.list <- as.integer(unlist(markers.list))
    cell.pop.id <- fcs.mat[,as.integer(annotation.column)]
    fcsg2.object <- NULL
    
    if(length(cell.pop.id)>0 && length(unique(cell.pop.id))<200 && !as.integer(annotation.column)%in%markers.list)
    {
        pop.ids <- sort(unique(cell.pop.id))
        
        mat.list <- list()
        position.list <- list()
        freq.list <- list()
        pattern.list <- list()
        for(i in 1:length(pop.ids))
        {
            mat <- fcs.mat[which(cell.pop.id==pop.ids[[i]]), ]
            m.list <- lapply(1:length(markers.list), function(j)
            {
                return( mean( mat[,markers.list[[j]]] ) )
            })
            sd.list <- lapply(1:length(markers.list), function(j)
            {
                s <- sd( mat[, markers.list[[j]]] )
                if(is.na(s))
                {
                    s <- 0
                }
                
                return(s)
            })
            freq.list[[i]] <- nrow(mat)/nrow(fcs.mat) * 100
            pattern.list[[i]] <- rep(0, length(markers.list))
            position.list[[i]] <- list(m.list, sd.list)
            mat.list[[i]] <- mat
        }
        
        fcsg2.object <- list("Frequencies" = freq.list,
                             "Expr" = mat.list,
                             "Positions" = position.list,
                             "Patterns" = pattern.list)
    }
    
    return(fcsg2.object)
    
}
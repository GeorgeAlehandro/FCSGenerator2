library(shiny)
library(shinydashboard)
library(shinyjs)
library(flowCore)
library(ggplot2)
library(d3r)
library(reshape2)
library(ggridges)
library(msm)
library(tcltk)
library(rlist)
library(stringr)

source("GenerateFCS.R")
source("SaveAndLoadFCS.R")
source("ModifyFCS.R")
source("MergeFiles.R")
source("TransformationAndCompensation.R")
source("FCSGframework.R")

Sys.setlocale('LC_ALL','C')

server <- function(input, output, session)
{
  fixUploadedFilesNames <- function(x) {
    if (is.null(x)) {
      return()
    }

    oldNames = x$datapath
    newNames = file.path(dirname(x$datapath),
                         x$name)
    file.rename(from = oldNames, to = newNames)
    x$datapath <- newNames
    x
  }
  useShinyjs()
  #0 - App variables and Functions
  #===================================================================
  app.variables <- reactiveValues(
    ref.objects = NULL, #REFERENCE OBJECTS + MODELS
    group.objects = NULL, #GROUPS GENERATED FROM A REFERENCE / MODEL
    temp.fcs.files = NULL,

    #TP
    TP.list = NULL,
    TP.objects = NULL,
    #==

    #MIX
    mix.objects = NULL,
    #==

    #Groups
    group.objects = NULL,
    #==

    log.text = NULL,
    init=T
  )

  env.var <- reactiveValues(
    tool.wd = getwd(), #system.file("shinyApp", "app", package = "FCSGenerator2"),
    zip.output = NULL
  )


  update.ref.sel <- function(tag.id)
  {
    shinyjs::disable(tag.id)

    ref.names <- list(NULL=NULL)
    if(!is.null(app.variables$ref.objects) && length(app.variables$ref.objects)>0)
    {
      ref.names <- 1:length(app.variables$ref.objects)
      names(ref.names) <- sapply(1:length(app.variables$ref.objects), function(i)
      {
        return(app.variables$ref.objects[[i]][["Name"]])
      })
    }
    updateSelectInput(session, tag.id, choices=ref.names, selected = ref.names)

    delay(50, shinyjs::enable(tag.id))
  }

  update.markers.sel <- function(tag.id, ref.id)
  {
    shinyjs::disable(tag.id)
    markers <- list(NULL=NULL)
    if(!is.null(input[[ref.id]]) && length(app.variables$ref.objects)>0)
    {
      id <- as.numeric(input[[ref.id]])
      if(!is.na(id) && id <= length(app.variables$ref.objects))
      {
        ref <- app.variables$ref.objects[[id]]
        if(length(ref)>0)
        {
          ref.name <- ref$Name
          markers <- ref$Markers
          names(markers) <- colnames(ref$FCSG2$Expr[[1]])[markers]
        }
      }
    }
    updateSelectInput(session, tag.id, choices=markers, selected = markers)

    delay(50, shinyjs::enable(tag.id))
  }

  update.populations.sel <- function(tag.id, ref.id)
  {
    shinyjs::disable(tag.id)

    populations <- list(NULL=NULL)
    if(!is.null(input[[ref.id]]) && length(app.variables$ref.objects)>0)
    {
      id <- as.numeric(input[[ref.id]])
      if(!is.na(id) && id <= length(app.variables$ref.objects))
      {
        ref <- app.variables$ref.objects[[id]]
        ref.name <- ref$Name
        if(length(ref)>0)
        {
          populations <- 1:length(ref$FCSG2$Expr)
        }
      }
    }
    updateSelectInput(session, tag.id, choices=populations, selected = populations)

    delay(50, shinyjs::enable(tag.id))
  }

  update.populations.from.temp <- function(ref.id, tag.id)
  {
    shinyjs::disable(tag.id)

    populations <- character(0)
    selected.populations <- NULL
    if(!is.null(input[[ref.id]]) && length(app.variables$ref.objects)>0)
    {
      id <- as.numeric(input[[ref.id]])
      if(!is.na(id) && id <= length(app.variables$ref.objects))
      {
        ref <- app.variables$ref.objects[[id]]
        ref.name <- ref$Name
        if(length(ref)>0)
        {
          if(length(app.variables$temp.objects)>0 && length(app.variables$temp.objects[[ref.name]])>0)
          {
            ref <- app.variables$temp.objects[[ref.name]]
          }
          populations <- 1:length(ref$FCSG2$Expr)
          selected.populations <- populations
        }
      }
    }
    if(!is.null(input[[tag.id]]) && input[[tag.id]]!="")
    {
      selected.populations <- input[[tag.id]]
    }
    updateSelectInput(session, tag.id, choices=populations, selected = selected.populations)

    delay(50, shinyjs::enable(tag.id))
  }



  #Global UI
  observe(
    {
      if(input$enable_comments)
      {
        shinyjs::show(selector="div.help_comment")
      }

      else
      {
        shinyjs::hide(selector="div.help_comment")
      }
    })

  observe(
    {
      if(!is.null(app.variables$ref.objects) && length(app.variables$ref.objects)>0)
      {
        shinyjs::show("global_files_div")
      }

      else
      {
        shinyjs::hide("global_files_div")
      }
    })

  output$global_files_list <- renderUI(
    {
      files.ui <- NULL
      if(!is.null(app.variables$ref.objects) && length(app.variables$ref.objects)>0)
      {
        files.ui <- lapply(1:length(app.variables$ref.objects), function(i)
        {
          nmb.events <- sum(sapply(app.variables$ref.objects[[i]]$FCSG2$Expr, function(m){return(nrow(m))}))
          tmp.ui <- tagList(
            fluidRow
            (
              column
              (
                width=1,
                checkboxInput(paste0("global_ref_object_",i,"_cb"), NULL, value = F)
              ),
              column
              (
                width=1,
                p(app.variables$ref.objects[[i]][["GroupSize"]], style="padding-top:1vh;")
              ),
              column
              (
                width = 3,style="overflow:auto",
                p(app.variables$ref.objects[[i]][["Name"]], style="padding-top:1vh;")
              ),
              column
              (
                width = 2,
                p(length(app.variables$ref.objects[[i]]$FCSG2$Expr), style="padding-top:1vh")
              ),
              column
              (
                width = 2,
                p(length(app.variables$ref.objects[[i]]$Markers), style="padding-top:1vh")
              ),
              column
              (
                width = 3,
                p(nmb.events, style="padding-top:1vh")
              )
            )
          )
          return(tmp.ui)
        })
      }
      return(files.ui)
    })

  observeEvent(input$global_files__rm,
               {
                 shinyjs::disable(paste0("global_files_rm"))
                 if(length(app.variables$ref.objects)>0)
                 {
                   selected.files <- NULL
                   for(i in 1:length(app.variables$ref.objects))
                   {
                     if(input[[paste0("global_ref_object_",i,"_cb")]])
                     {
                       selected.files <- c(selected.files, i)
                     }
                   }
                   if(length(selected.files)>0)
                   {
                     progress <- Progress$new()
                     progress$set(message = "Removing Files", value = 0)
                     for(i in selected.files)
                     {
                       ref.object <- app.variables$ref.objects[[i]]
                       if(length(ref.object)>0)
                       {
                         if(ref.object$Name%in%names(app.variables$temp.objects))
                         {
                           id <- which(names(app.variables$temp.objects)==ref.object$Name)[[1]]
                           app.variables$temp.objects <- app.variables$temp.objects[-id]
                         }
                         if(ref.object$Name%in%names(app.variables$group.objects))
                         {
                           id <- which(names(app.variables$group.objects)==ref.object$Name)[[1]]
                           app.variables$group.objects <- app.variables$group.objects[-id]
                         }
                         progress$inc(1/length(selected.files), detail=paste0(ref.object$Name, " removed"))
                       }
                     }
                     if(length(app.variables$ref.objects)==length(selected.files))
                     {
                       app.variables$ref.objects <- NULL
                     }
                     else
                     {
                       app.variables$ref.objects <- app.variables$ref.objects[-selected.files]
                     }
                     progress$set(message = "Files Removed", value=1)
                     delay(500, progress$close())
                   }
                 }
                 delay(500, shinyjs::enable(paste0("global_files_rm")))
               })

  observeEvent(input$global_files_sel_all,
               {
                 if(length(app.variables$ref.objects)>0)
                 {
                   for(i in 1:length(app.variables$ref.objects))
                   {
                     ref <- app.variables$ref.objects[[i]]
                     if(length(ref)>0)
                     {
                       updateCheckboxInput(session, paste0("global_ref_object_",i,"_cb"), value = T)
                     }
                   }
                 }
               })

  observeEvent(input$global_files_desel_all,
               {
                 if(length(app.variables$ref.objects)>0)
                 {
                   for(i in 1:length(app.variables$ref.objects))
                   {
                     ref <- app.variables$ref.objects[[i]]
                     if(length(ref)>0)
                     {
                       updateCheckboxInput(session, paste0("global_ref_object_",i,"_cb"), value = F)
                     }
                   }
                 }
               })
  #===================================================================





  #1 - Add Reference Files
  #===================================================================
  observe( #UPDATE DEFAULT FILENAME
    {
      if(input$t_1_generated_name == "filename" || input$t_1_generated_name == paste0("model",length(app.variables$ref.objects)))
      {
        updateTextInput(session, "t_1_generated_name", value="model")
      }
    })

  observeEvent(input$t_1_files_rm, #DELETE REFERENCE FILES
               {
                 shinyjs::disable("t_1_files_rm")
                 if(length(input$t_1_files_rm_sel)>0 && !is.null(input$t_1_files_rm_sel))
                 {
                   progress <- Progress$new()
                   progress$set(message = "Removing Files", value = 0)
                   ids <- as.numeric(input$t_1_files_rm_sel)
                   for(i in ids)
                   {
                     ref.object <- app.variables$ref.objects[[i]]
                     if(length(ref.object)>0)
                     {
                       if(ref.object$Name%in%names(app.variables$temp.objects))
                       {
                         id <- which(names(app.variables$temp.objects)==ref.object$Name)[[1]]
                         app.variables$temp.objects <- app.variables$temp.objects[-id]
                       }
                       if(ref.object$Name%in%names(app.variables$shift.objects))
                       {
                         id <- which(names(app.variables$shift.objects)==ref.object$Name)[[1]]
                         app.variables$shift.objects <- app.variables$shift.objects[-id]
                       }
                       if(ref.object$Name%in%names(app.variables$TP.objects))
                       {
                         id <- which(names(app.variables$TP.objects)==ref.object$Name)[[1]]
                         app.variables$TP.objects <- app.variables$TP.objects[-id]
                       }
                       if(ref.object$Name%in%names(app.variables$mix.objects))
                       {
                         id <- which(names(app.variables$mix.objects)==ref.object$Name)[[1]]
                         app.variables$mix.objects <- app.variables$mix.objects[-id]
                       }
                       progress$inc(1/length(ids), detail=paste0(ref.object$Name, " removed"))
                     }
                   }
                   if(length(app.variables$ref.objects)==length(ids))
                   {
                     app.variables$ref.objects <- NULL
                   }
                   else
                   {
                     app.variables$ref.objects <- app.variables$ref.objects[-ids]
                   }
                   progress$set(message = "Files Removed", value=1)
                   delay(500, progress$close())
                 }
                 delay(500, shinyjs::enable("t_1_files_rm"))
               })

  output$t_1_pop_list <- renderUI(
    {
      pop.ui <- list()
      if(as.numeric(input$t_1_nmb_populations)>0)
      {
        pop.ui <- lapply(1:(as.numeric(input$t_1_nmb_populations)), function(i)
        {
          effective.perc <- 100
          val <- tagList(
            column(
              width=1
            ),
            column(
              width=2,
              p(i, style="padding-top:10%")
            ),
            column(
              width=4,
              numericInput(paste0("t_1_pop_",i,"min__freq"), NULL,
                           value = trunc(effective.perc/as.numeric(input$t_1_nmb_populations)*100)/100)
            ),
            column(
              width=5,
              numericInput(paste0("t_1_pop_",i,"max__freq"), NULL,
                           value = trunc(effective.perc/as.numeric(input$t_1_nmb_populations)*100)/100+1)
            )
          )
          return(val)
        })
        pop.ui <- tagList(
          tagList(
            column(
              width=3,
              p(tags$b("Population ID"))
            ),
            column(
              width=4,
              p(tags$b("Min Frequency"))
            ),
            column(
              width=5,
              p(tags$b("Max Frequency"))
            )
          ),
          pop.ui
        )
        return(pop.ui)
      }
      return(pop.ui)
    })

  output$t_1_files_main <- renderUI(
    {
      files.ui <- NULL
      if(length(app.variables$ref.objects)>0)
      {
        files.ui <- lapply(1:length(app.variables$ref.objects), function(id)
        {
          ref.object <- app.variables$ref.objects[[id]]
          pop.col <- ref.object$AnnotationColumn
          #==
          visualized.markers <- ref.object$Markers
          names(visualized.markers) <- colnames(ref.object$Expr[[1]])[visualized.markers]
          #==
          tmp.ui <- tagList(
            div(
              id=paste0("t_1_ref_",id,"_fr"),
              shinydashboard::box(
                width = 12, collapsible=T, style="min-height:15vh", collapsed = T,
                title = ref.object$Name,
                style="padding-left:5%",
                plotlyOutput(paste0("t_1_file_",id), width = "100%")
              )
            )
          )

          markers <- ref.object$Markers
          annotation.column <- ref.object$AnnotationColumn

          pop.mat <- matrix(nrow=length(ref.object$FCSG2$Expr), ncol=length(ref.object$Markers))
          colnames(pop.mat) <- colnames(ref.object$FCSG2$Expr[[1]])[markers]
          rownames(pop.mat) <- rep("", nrow(pop.mat))

          for(i in 1:length(ref.object$FCSG2$Expr))
          {
            mat.means <- ref.object$FCSG2$Positions[[i]][[1]]
            rownames(pop.mat)[i] <- ref.object$FCSG2$Expr[[i]][1,annotation.column]
            for(j in markers)
            {
              pop.mat[i,j] <- mat.means[[j]]
            }
          }

          pop.plot <- heatmaply(pop.mat, Rowv = T, Colv="Rowv", dendrogram = "none", limits = c(-0.5,4.5))
          output[[paste0("t_1_file_",id)]] <- renderPlotly(pop.plot)

          return(tmp.ui)
        })
      }
      return(files.ui)
    })

  observeEvent(input$t_1_create,  #CREATE FILES
               {
                 shinyjs::disable("t_1_create")
                 if(as.numeric(input$t_1_nmb_files)>0)
                 {
                   nmb.files <- as.numeric(input$t_1_nmb_files)
                   nmb.populations <- as.numeric(input$t_1_nmb_populations)
                   nmb.events <- as.numeric(input$t_1_nmb_events)
                   nmb.markers <- as.numeric(input$t_1_nmb_markers)

                   if(nmb.populations>0 & nmb.events>0 & nmb.markers>0)
                   {
                     if(nmb.populations > 2^nmb.markers)
                     {
                       tmp.message <- paste("Error: to generate", nmb.populations, "populations, you need at least",
                                            as.integer(log(nmb.populations,2))+1, "markers")
                       showNotification(tmp.message, duration = 5, type="error")
                     }
                     else
                     {
                       progress <- Progress$new()
                       progress$set(message = "Generating Files", value = 0)
                       # update.log("GENERATING FILES")
                       nmb.markers <- input$t_1_nmb_markers
                       pop.names <- lapply(1:nmb.populations, function(pop)
                       {
                         return(input[[paste0("t_1_pop_",pop,"_name")]])
                       })


                       min.freq.list <- sapply(1:nmb.populations, function(i)
                       {
                         return(as.numeric(input[[paste0("t_1_pop_",i,"min__freq")]]))
                       })

                       max.freq.list <- sapply(1:nmb.populations, function(i)
                       {
                         return(as.numeric(input[[paste0("t_1_pop_",i,"max__freq")]]))
                       })
                       if(!(sum(min.freq.list)<=100 && sum(max.freq.list)>=100))
                       {
                         progress$close()
                         if(sum(min.freq.list)>100)
                         {
                           showNotification("The Sum of the min frequencies must be equal or lower to 100", duration = 5, type="error")
                         }
                         if(sum(max.freq.list)<100)
                         {
                           showNotification("The Sum of the max frequencies must be greater or equal to 100", duration = 5, type="error")
                         }
                       }
                       else
                       {
                         for(current.file in 1:nmb.files)
                         {
                           tmp.name <- input$t_1_generated_name
                           if(is.null(tmp.name) || is.na(tmp.name))
                           {
                             tmp.name <- paste0("GEN__",nmb.events,"_",
                                                nmb.markers,"_",nmb.populations)
                           }
                           tmp.name <- paste0(tmp.name, "_", current.file)

                           x <- generate.FCSG2(nmb.events, nmb.markers, nmb.populations, min.freq.list, max.freq.list)
                           if(length(app.variables$ref.objects)==0)
                           {
                             app.variables$ref.objects <- list()
                           }
                           app.variables$ref.objects <- list.append(app.variables$ref.objects,
                                                                    as.list(list("FCSG2"=x,
                                                                                 "Transformed"=T,
                                                                                 "Compensated"=T,
                                                                                 "Name"=tmp.name,
                                                                                 "AnnotationColumn"=nmb.markers+2,
                                                                                 "Markers"=c(1:nmb.markers),
                                                                                 "GroupSize"=1)))


                           progress$inc(1/nmb.files, detail = paste(tmp.name, "generated"))
                           # update.log(paste("========", tmp.name, "generated"))
                         }
                         progress$set(message = "Files generated", value = 1)
                         # update.log("======== FILES GENERATED")
                         # update.log("")
                         delay(700, progress$close())
                       }
                     }
                   }
                 }

                 delay(500, shinyjs::enable("t_1_create"))
               })

  observeEvent(input$t_1_select,  #SELECT FILES
               {
                 shinyjs::disable("t_1_select")
                 m <- matrix(nrow=1,ncol=2)
                 m[1,1] = "FlowFrames"
                 m[1,2] = "*.csv;*.fcs"
                 #==
                 #temp.files <- tk_choose.files(filters = m,multi = T)
                 added_files <- fixUploadedFilesNames(input$t_1_select)
                 temp.files <- added_files$datapath
                 if(length(temp.files) > 0)
                 {
                   progress <- Progress$new()
                   progress$set(message = "Loading Files", value = 0)
                   # update.log("LOADING Files")
                   lapply(temp.files, function(f)
                   {
                     l <- length(f)
                     x <- NULL
                     mark.list <- list()
                     if(grepl("csv",f))
                     {
                       x <- as.matrix(read.csv(f))
                       x <- flowFrame(x)
                       lapply(1:ncol(x@exprs), function(i)
                       {
                         nx <- x@description[[paste0("$P",i,"S")]]
                         if(!is.null(nx) && !is.na(nx) && nx != "" && nx != " ")
                         {
                           mark.list[[i]] <<- nx
                         }
                         else
                         {
                           mark.list[[i]] <<- colnames(x)[i]
                         }
                       })
                     }
                     else
                     {
                       x <- read.FCS(f,emptyValue = FALSE)
                       lapply(1:ncol(x@exprs), function(i)
                       {
                         nx <- x@description[[paste0("$P",i,"S")]]
                         if(!is.null(nx) && !is.na(nx) && nx != "" && nx != " ")
                         {
                           mark.list[[i]] <<- nx
                         }
                         else
                         {
                           mark.list[[i]] <<- colnames(x)[i]
                         }
                       })
                     }

                     tmp.name <- paste0(basename(substr(f,1,nchar(f)-4)))
                     progress$inc(1/length(temp.files), detail = paste(f, "loaded"))
                     if(length(app.variables$temp.fcs.files)>0)
                     {
                       app.variables$temp.fcs.files[[length(app.variables$temp.fcs.files)+1]] <- list("FCS"=x,
                                                                                                      "Name"=tmp.name)
                     }
                     else
                     {
                       app.variables$temp.fcs.files <- list()
                       app.variables$temp.fcs.files[[1]] <- list("FCS"=x,
                                                                 "Name"=tmp.name)
                     }
                     # update.log(paste("========",f, "loaded"))
                   })
                   progress$set(message = "Files loaded", value = 1)
                   # update.log("======== FILES LOADED")
                   # update.log("")
                   delay(700, progress$close())
                 }
                 else
                 {
                   showNotification("NO FILES SELECTED", duration=5, type="error")
                 }

                 delay(500, shinyjs::enable("t_1_select"))
               })

  output$t_1_fcs_list <- renderUI(
    {
      fcs.ui <- list()
      if(length(app.variables$temp.fcs.files)>0)
      {
        shinyjs::show("t_1_validate")
        shinyjs::enable("t_1_validate")
        fcs.ui <- lapply(1:length(app.variables$temp.fcs.files), function(i)
        {
          x <- app.variables$temp.fcs.files[[i]]
          markers <- 1:ncol(x[["FCS"]]@exprs)
          names(markers) <- colnames(x[["FCS"]]@exprs)

          tmp.name <- x$Name
          if(!is.null(input[[paste0("t_1_imported_name_",i)]]) && !is.na(input[[paste0("t_1_imported_name_",i)]]))
          {
            tmp.name <- input[[paste0("t_1_imported_name_",i)]]
          }
          val <- tagList(
            fluidRow
            (
              style="min-height:10vh",
              column
              (
                width=12,
                column(
                  width=4,
                  textInput(paste0("t_1_imported_name_",i), "Filename", value=tmp.name)
                ),
                column(
                  width=4,
                  selectInput(paste0("t_1_fcs_",i,"_annotation_column"), "Annotation Column", multiple = F, choices=markers)
                ),
                column(
                  width=4,
                  selectInput(paste0("t_1_fcs_",i,"_markers_list"), "Markers List", multiple = T, choices=markers, selected = markers)
                )
              )
            )
          )
          return(val)
        })
      }
      else
      {
        shinyjs::hide("t_1_validate")
        shinyjs::disable("t_1_validate")
      }
      return(fcs.ui)
    })

  observeEvent(input$t_1_fcs_validate,
               {
                 shinyjs::disable("t_1_fcs_validate")
                 if(length(app.variables$temp.fcs.files)>0)
                 {
                   progress <- Progress$new()
                   progress$set(message = "Importing FCS Files", value = 0)

                   nmb.files <- isolate(length(app.variables$temp.fcs.files))
                   for(i in 1:nmb.files)
                   {
                     fcs.obj <- app.variables$temp.fcs.files[[nmb.files-i+1]]
                     annotation.column <- as.numeric(input[[paste0("t_1_fcs_",i,"_annotation_column")]])
                     markers.list <- as.numeric(input[[paste0("t_1_fcs_",i,"_markers_list")]])
                     if(!is.null(annotation.column) & length(markers.list)>0)
                     {
                       x <- load.annotated.FCS.as.object(fcs.obj$FCS, annotation.column, markers.list)
                       if(!is.null(x))
                       {
                         if(length(app.variables$ref.objects)==0)
                         {
                           app.variables$ref.objects <- list()
                         }
                         tmp.name <- input[[paste0("t_1_imported_name_",i)]]
                         if(is.null(tmp.name) || is.na(tmp.name))
                         {
                           tmp.name <- fcs.obj$Name
                         }
                         app.variables$ref.objects <- list.append(app.variables$ref.objects,
                                                                  as.list(list("FCSG2"=x,
                                                                               "Transformed"=F,
                                                                               "Compensated"=F,
                                                                               "Name"=tmp.name,
                                                                               "AnnotationColumn"=annotation.column,
                                                                               "Markers"=markers.list,
                                                                               "GroupSize"=1)))
                         if(length(app.variables$temp.fcs.files)>1)
                         {
                           app.variables$temp.fcs.files <- app.variables$temp.fcs.files[-(nmb.files-i+1)]
                         }
                         else
                         {
                           app.variables$temp.fcs.files <- NULL
                         }
                       }
                       else
                       {
                         showNotification("The file was empty. Please Select a different Annotation Column and make sure it is not in the markers list",
                                          duration = 5, type="error")
                       }
                     }

                     progress$inc(1/length(app.variables$temp.fcs.files), detail = paste(fcs.obj$Name, "imported"))
                   }
                   progress$set(message = "FCS files imported", value = 1)
                   delay(700, progress$close())
                 }
                 delay(500, shinyjs::enable("t_1_fcs_validate"))
               })
  #===================================================================






  #2 - Compensate, Transform
  #===================================================================
  output$t_2_files <- renderUI(
    {
      files.ui <- NULL
      if(length(app.variables$ref.objects)>0)
      {
        files.ui <- lapply(1:length(app.variables$ref.objects), function(i)
        {
          tmp.ui <- NULL

          ref <- app.variables$ref.objects[[i]]
          if(length(ref)>0)
          {
            comp <- p("False", style="color:red")
            if(ref$Compensated)
            {
              comp <- p("True", style="color:green")
            }
            transf <- p("False", style="color:red")
            if(ref$Transformed)
            {
              transf <- p("True", style="color:green")
            }

            tmp.ui <- tagList(
              fluidRow
              (
                style="margin-bottom:2%",
                column
                (
                  width=2,
                  checkboxInput(paste0("t_2_file_",i), "", value = F)
                ),
                column
                (
                  width=4,
                  h5(ref$Name)
                ),
                column
                (
                  width=3,
                  tags$b(comp)
                ),
                column
                (
                  width=3,
                  tags$b(transf)
                )
              )
            )
          }

          return(tmp.ui)
        })
      }

      return(files.ui)
    })

  output$t_2_transform_param <- renderUI(
    {
      param.ui <- NULL
      if(!is.null(input$t_2_transform_sel) && as.integer(input$t_2_transform_sel) == 2)
      {
        param.ui <- numericInput("t_2_arcsinh_w", "W", value=5)
      }
      return(param.ui)
    })

  observeEvent(input$t_2_compensate,
               {
                 shinyjs::disable("t_2_compensate")
                 if(length(app.variables$ref.objects)>0)
                 {
                   progress <- Progress$new()
                   progress$set(message = "Compensating Selection", value = 0)

                   selected.ref <- sapply(1:length(app.variables$ref.objects), function(i)
                   {
                     return(as.logical(input[[paste0("t_2_file_",i)]]))
                   })

                   selected.ref <- unlist(which(selected.ref))
                   if(length(selected.ref)>0)
                   {
                     for(i in selected.ref)
                     {
                       ref <- app.variables$ref.objects[[i]]
                       if(length(ref)>0)
                       {
                         tmp.fcs <- save.object.as.FCS(ref$FCSG2)
                         tmp.fcs <- m.compensate(tmp.fcs)
                         tmp.obj <- load.annotated.FCS.as.object(fcs = tmp.fcs, annotation.column = ref$AnnotationColumn, markers.list = ref$Markers)

                         app.variables$ref.objects[[i]]$FCSG2 <- tmp.obj
                         app.variables$ref.objects[[i]]$Compensated <- T
                         progress$inc(1/length(selected.ref), detail=paste0(ref$Name, " compensated"))
                       }
                     }
                   }
                   progress$set(message = "Files compensated", value = 1)
                   shinyjs::delay(500, progress$close())
                 }
                 delay(500, shinyjs::enable("t_2_compensate"))
               })

  observeEvent(input$t_2_transform,
               {
                 shinyjs::disable("t_2_transform")
                 if(length(app.variables$ref.objects)>0)
                 {
                   progress <- Progress$new()
                   progress$set(message = "Transforming Selection", value = 0)
                   selected.ref <- sapply(1:length(app.variables$ref.objects), function(i)
                   {
                     return(as.logical(input[[paste0("t_2_file_",i)]]))
                   })

                   selected.ref <- unlist(which(selected.ref))
                   if(length(selected.ref)>0)
                   {
                     for(i in selected.ref)
                     {
                       ref <- app.variables$ref.objects[[i]]
                       if(length(ref)>0)
                       {
                         tmp.fcs <- save.object.as.FCS(ref$FCSG2)
                         if(!is.null(input$t_2_transform_sel))
                         {
                           if(as.integer(input$t_2_transform_sel) == 1)
                           {
                             tmp.fcs <- m.transform.logicle(tmp.fcs, colnames(ref$FCSG2$Expr[[1]])[ref$Markers])
                           }
                           else if(as.integer(input$t_2_transform_sel) == 2)
                           {
                             w <- as.numeric(input[["t_2_arcsinh_w"]])
                             tmp.fcs <- m.transform.asinh(tmp.fcs, colnames(ref$FCSG2$Expr[[1]])[ref$Markers], w)
                           }
                         }
                         tmp.obj <- load.annotated.FCS.as.object(fcs = tmp.fcs, annotation.column = ref$AnnotationColumn, markers.list = ref$Markers)

                         app.variables$ref.objects[[i]]$FCSG2 <- tmp.obj
                         app.variables$ref.objects[[i]]$Transformed <- T
                         progress$inc(1/length(selected.ref), detail=paste0(ref$Name, " transformed"))
                       }
                     }
                   }
                   progress$set(message = "Files Transformed", value = 1)
                   shinyjs::delay(500, progress$close())
                 }
                 delay(500, shinyjs::enable("t_2_compensate"))
               })

  observe( #SELECT ALL
    {
      if(length(app.variables$ref.objects)>0)
      {
        if(as.logical(input$t_2_file_select_all))
        {
          for(i in 1:length(app.variables$ref.objects))
          {
            updateCheckboxInput(session, paste0("t_2_file_",i), value = T)
          }
        }
        else
        {
          for(i in 1:length(app.variables$ref.objects))
          {
            updateCheckboxInput(session, paste0("t_2_file_",i), value = F)
          }
        }
      }
    })
  #===================================================================






  #3.1 - Generate Model (POP MODIFICATION)
  #===================================================================
  output$t_3_pop_ui <- renderUI( #UI BODY
    {
      body.ui <- NULL
      if(!is.null(input$t_3_pop_ref_sel))
      {
        id <- as.numeric(input$t_3_pop_ref_sel)
        ref <- app.variables$ref.objects[[id]]

        if(length(ref)>0)
        {
          ref.name <- ref$Name
          if(length(app.variables$temp.objects)==0)
          {
            app.variables$temp.objects <- list()
          }
          if( !ref.name%in%names(app.variables$temp.objects) )
          {
            app.variables$temp.objects[[ref.name]] <- ref

            app.variables$temp.objects[[ref.name]]$OriginExpr <-
              lapply(ref$FCSG2$Expr, function(mat){return(mat)})

            app.variables$temp.objects[[ref.name]]$TempFrequencies <- rep(100, length(ref$FCSG2$Frequencies))
          }
          ref <- app.variables$temp.objects[[ref.name]]

          annotation.column <- ref$AnnotationColumn
          markers <- as.numeric(input$t_3_pop_move_markers_sel)
          population <- as.numeric(input$t_3_pop_move_pop_sel)

          body.ui <- lapply(markers, function(i)
          {
            pos.m <- ref$FCSG2$Positions[[population]][[1]][[i]]
            pos.sd <- ref$FCSG2$Positions[[population]][[2]][[i]]

            collapsed <- T
            if(i==markers[[length(markers)]])
            {
              collapsed <- F
            }

            tmp.ui <- tagList(
              column(
                width=6,
                h4(colnames(ref$FCSG2$Expr[[1]])[i]),

                fluidRow(
                  width=12,
                  column(
                    width=12,
                    plotOutput(paste0("move_",i,"_plot"))
                  ),
                  column(
                    width=12,
                    sliderInput(paste0("move_",i,"_plot_m"), "Mean", min = -0.5, max=4.5, value = pos.m, step = 0.01)
                  ),
                  column(
                    width=12,
                    sliderInput(paste0("move_",i,"_plot_sd"), "SD", min = 0, max=0.8, value = pos.sd, step = 0.0002)
                  )
                )
              )
            )

            return(tmp.ui)
          })
        }
      }

      return(body.ui)
    })



  #GENERATE AND UPDATE THE PLOTS
  observeEvent(input$t_3_pop_move_pop_sel,
               {
                 if(!is.null(input$t_3_pop_ref_sel))
                 {
                   id <- as.numeric(input$t_3_pop_ref_sel)
                   if(!is.null(id) && !is.na(id) && id <= length(app.variables$ref.objects))
                   {
                     ref <- app.variables$ref.objects[[id]]

                     if(length(ref)>0)
                     {
                       ref.name <- ref$Name
                       if(length(app.variables$temp.objects)==0)
                       {
                         app.variables$temp.objects <- list()
                       }
                       if( !ref.name%in%names(app.variables$temp.objects) )
                       {
                         app.variables$temp.objects[[ref.name]] <- ref

                         app.variables$temp.objects[[ref.name]]$OriginExpr <-
                           lapply(ref$FCSG2$Expr, function(mat){return(mat)})

                         app.variables$temp.objects[[ref.name]]$TempFrequencies <- rep(100, length(ref$FCSG2$Frequencies))
                       }
                       ref <- app.variables$temp.objects[[ref.name]]

                       annotation.column <- ref$AnnotationColumn
                       markers <- as.numeric(input$t_3_pop_move_markers_sel)
                       population <- as.numeric(input$t_3_pop_move_pop_sel)
                       if(!is.null(population) && !is.na(population) && population <= length(ref$FCSG2$Positions))
                       {
                         body.ui <- lapply(markers, function(i)
                         {
                           pos.m <- ref$FCSG2$Positions[[population]][[1]][[i]]
                           pos.sd <- ref$FCSG2$Positions[[population]][[2]][[i]]

                           if( length(pos.m)>0 && length(pos.sd)>0 )
                           {
                             updateSliderInput(session, paste0("move_",i,"_plot_m"), value=pos.m)
                             updateSliderInput(session, paste0("move_",i,"_plot_sd"), value=pos.sd)
                           }
                         })
                       }
                       app.variables$temp.objects[[ref.name]] <- ref
                     }
                   }
                 }
               })

  observe( #CHANGE THE POSITIONS
    {
      body.ui <- NULL
      if(!is.null(input$t_3_pop_ref_sel))
      {
        id <- as.numeric(input$t_3_pop_ref_sel)
        if(!is.null(id) && !is.na(id) && id <= length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]

          if(length(ref)>0)
          {
            ref.name <- ref$Name
            if(length(app.variables$temp.objects)==0)
            {
              app.variables$temp.objects <- list()
            }
            if( !ref.name%in%names(app.variables$temp.objects) )
            {
              app.variables$temp.objects[[ref.name]] <- ref

              app.variables$temp.objects[[ref.name]]$OriginExpr <-
                lapply(ref$FCSG2$Expr, function(mat){return(mat)})

              app.variables$temp.objects[[ref.name]]$TempFrequencies <- rep(100, length(ref$FCSG2$Frequencies))
            }
            ref <- app.variables$temp.objects[[ref.name]]

            annotation.column <- ref$AnnotationColumn
            markers <- as.numeric(input$t_3_pop_move_markers_sel)
            population <- as.numeric(input$t_3_pop_move_pop_sel)
            if(!is.null(population) && !is.na(population) && population <= length(ref$FCSG2$Positions))
            {
              body.ui <- lapply(markers, function(i)
              {
                old.pos.m <- ref$FCSG2$Positions[[population]][[1]][[i]]
                old.pos.sd <- ref$FCSG2$Positions[[population]][[2]][[i]]

                pos.m <- as.numeric(input[[paste0("move_",i,"_plot_m")]])
                pos.sd <- as.numeric(input[[paste0("move_",i,"_plot_sd")]])
                if( length(pos.m)>0 && length(pos.sd)>0 )
                {
                  if( !(pos.m==old.pos.m) || !(pos.sd==old.pos.sd) )
                  {
                    shinyjs::disable(paste0("move_",i,"_plot_m"))
                    shinyjs::disable(paste0("move_",i,"_plot_sd"))

                    ref$FCSG2 <<- move.population(ref$FCSG2, population, i, c(pos.m, pos.sd), -0.5, 4.5, markers)
                    ref$OriginExpr[[population]] <<- ref$FCSG2$Expr[[population]]

                    delay(10, shinyjs::enable(paste0("move_",i,"_plot_m")))
                    delay(10, shinyjs::enable(paste0("move_",i,"_plot_sd")))
                  }
                }
              })
            }
            app.variables$temp.objects[[ref.name]] <- ref
          }
        }
      }

      return(body.ui)
    })

  observe( #UPDATE THE MFI PLOTS
    {
      markers.ui <- NULL

      if(!is.null(input$t_3_pop_ref_sel))
      {
        id <- as.numeric(input$t_3_pop_ref_sel)
        if(!is.na(id) && id<=length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]

          markers <- as.numeric(input$t_3_pop_move_markers_sel)
          population <- as.numeric(input$t_3_pop_move_pop_sel)

          if(length(ref)>0 && population%in%(1:length(ref$FCSG2$Expr)))
          {
            ref.name <- ref$Name
            if(length(app.variables$temp.objects)==0)
            {
              app.variables$temp.objects <- list()
            }
            if( !ref.name%in%names(app.variables$temp.objects) )
            {
              app.variables$temp.objects[[ref.name]] <- ref

              app.variables$temp.objects[[ref.name]]$OriginExpr <-
                lapply(ref$FCSG2$Expr, function(mat){return(mat)})

              app.variables$temp.objects[[ref.name]]$TempFrequencies <- rep(100, length(ref$FCSG2$Frequencies))
            }
            ref <- app.variables$temp.objects[[ref.name]]
            annotation.column <- ref$AnnotationColumn
            lapply(markers, function(i)
            {
              output[[paste0("move_",i,"_plot")]] <- renderPlot(
                {
                  tmp.plot <- NULL
                  x <- ref$FCSG2

                  mat <- NULL
                  pop.type <-NULL
                  for(j in 1:length(x$Expr))
                  {
                    mat <- c(mat, x$Expr[[j]][,i])
                  }
                  mat.pop <- x$Expr[[population]][,i]

                  df <- data.frame("val"=c(mat.pop, mat),
                                   "Population"=as.factor(c(rep(paste("Pop",population), length(mat.pop)), rep("All", length(mat)))))
                  colnames(df)[1] <- colnames(x$Expr[[1]])[i]

                  tmp.plot <- ggplot(df, aes(df[,1], fill=Population, colour=Population)) +
                    geom_histogram(bins = 100, position="identity", alpha=0.5) + theme(legend.position="top") +
                    xlab(colnames(x$Expr[[1]])[i])

                  return(tmp.plot)

                })
            })
          }
        }
      }

    })




  #UPDATE THE NEW SIZE OF THE POPULATIONS
  observe(
    {
      if(!is.null(input$t_3_pop_ref_sel))
      {
        id <- as.numeric(input$t_3_pop_ref_sel)
        if(!is.null(id) && !is.na(id) && id <= length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]

          if(length(ref)>0)
          {
            ref.name <- ref$Name
            annotation.column <- ref$AnnotationColumn
            markers <- ref$Markers
            population <- as.numeric(input$t_3_pop_size_pop_sel)

            if(length(app.variables$temp.objects)==0)
            {
              app.variables$temp.objects <- list()
            }
            if(is.null(app.variables$temp.objects[[ref.name]]))
            {
              app.variables$temp.objects[[ref.name]] <- ref

              app.variables$temp.objects[[ref.name]]$OriginExpr <-
                lapply(ref$FCSG2$Expr, function(mat){return(mat)})

              app.variables$temp.objects[[ref.name]]$TempFrequencies <- rep(100, length(ref$FCSG2$Frequencies))
            }
            ref <- app.variables$temp.objects[[ref.name]]

            new.freq <- as.numeric(input$t_3_pop_size_text)
            if(!is.null(population) && !is.na(population) && population <= length(ref$FCSG2$Expr) && population <= length(ref$TempFrequencies))
            {
              if(!is.null(new.freq) && !is.na(new.freq))
              {
                old.freq <- ref$TempFrequencies[[population]]
                if(abs(new.freq - old.freq))
                {
                  shinyjs::disable("t_3_pop_size_slider")
                  shinyjs::disable("t_3_pop_size_text")
                  shinyjs::disable("t_3_pop_size_pop_sel")

                  x <- ref$FCSG2
                  x$Expr[[population]] <- ref$OriginExpr[[population]]
                  x <- change.population.size(ref.object = x, pop.id = population, new.size = new.freq,
                                              min.val = -0.5, max.val = 4.5,
                                              annotation.column = annotation.column, marker.columns = markers)
                  ref$FCSG2 <- x
                  ref$TempFrequencies[[population]] <- new.freq
                  app.variables$temp.objects[[ref.name]] <- ref

                  delay(10, shinyjs::enable("t_3_pop_size_slider"))
                  delay(10, shinyjs::enable("t_3_pop_size_text"))
                  delay(10, shinyjs::enable("t_3_pop_size_pop_sel"))
                }
              }
            }
          }
        }

      }
    })




  #2 MARKERS VISUALIZATION
  output$t_3_pop_plot <- renderPlot(
    {
      markers.plot <- NULL
      if(!is.null(input$t_3_pop_ref_sel))
      {
        m1 <- as.integer(input[["t_3_pop_m1"]])
        m2 <- as.integer(input[["t_3_pop_m2"]])

        id <- as.numeric(input$t_3_pop_ref_sel)
        if(!is.null(id) && !is.na(id) && id <= length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]
          if(length(ref)>0 && !is.null(m1) && !is.null(m2))
          {
            ref.name <- ref$Name

            annotation.column <- ref$AnnotationColumn
            highlighted.population <- as.numeric(input$t_3_pop_hp)

            if(length(app.variables$temp.objects)==0)
            {
              app.variables$temp.objects <- list()
            }
            if( !ref.name%in%names(app.variables$temp.objects) )
            {
              app.variables$temp.objects[[ref.name]] <- ref

              app.variables$temp.objects[[ref.name]]$OriginExpr <-
                lapply(ref$FCSG2$Expr, function(mat){return(mat)})

              app.variables$temp.objects[[ref.name]]$TempFrequencies <- rep(100, length(ref$FCSG2$Frequencies))
            }
            ref <- app.variables$temp.objects[[ref.name]]

            if(input$t_3_pop_tab == "C")
            {
              population <- as.integer(input$t_3_pop_size_pop_sel)
              if(!is.null(population) && !is.na(population))
              {
                output$t_3_pop_size_nmb_events <- renderUI(
                  {
                    events.ui <- NULL
                    events.ui <- h5(nrow(ref$FCSG2$Expr[[population]]))

                    return(events.ui)
                  })
              }
            }

            if( !(m1==m2) )
            {
              x <- ref$FCSG2

              mat <- NULL
              pop.type <- NULL
              for(i in 1:length(x$Expr))
              {
                if(i%in%highlighted.population)
                {
                  pop.type <- c(pop.type, rep("Highlighted", nrow(x$Expr[[i]])))
                }
                else
                {
                  pop.type <- c(pop.type, rep("Others", nrow(x$Expr[[i]])))
                }
                mat <- rbind(mat, x$Expr[[i]])
              }

              df <- as.data.frame(mat)[,c(m1,m2)]
              colnames(df) <- colnames(x$Expr[[1]])[c(m1,m2)]
              df$Population <- as.factor(pop.type)
              pop.colors <- c(Highlighted="green", Others="black")

              markers.plot <- ggplot(df, aes(df[,1], df[,2], colour=Population)) +
                geom_point(size=0.8) + scale_colour_manual(values=pop.colors) + xlim(c(-0.5,4.5)) + ylim(c(-0.5,4.5)) +
                xlab(colnames(df)[1]) + ylab(colnames(df)[2])
            }
          }
        }
      }

      return(markers.plot)
    })



  #UPDATE NUMERIC INPUT / SLIDER WHEN CHANGING POP AND WHEN CHANGING ONE OF THEM
  observeEvent(input$t_3_pop_size_slider,
               {
                 old.value <- NULL
                 if(!is.null(input$t_3_pop_size_slider))
                 {
                   if(!is.null(input$t_3_pop_size_text))
                   {
                     old.value <- as.numeric(input$t_3_pop_size_text)
                   }

                   new.value <- as.numeric(input$t_3_pop_size_slider)
                   if( abs(new.value - old.value)>0.000001 )
                   {
                     updateNumericInput(session, "t_3_pop_size_text", value = new.value)
                   }
                 }
               })

  observeEvent(input$t_3_pop_size_text,
               {
                 old.value <- NULL
                 if(!is.null(input$t_3_pop_size_text))
                 {
                   if(!is.null(input$t_3_pop_size_slider))
                   {
                     old.value <- as.numeric(input$t_3_pop_size_slider)
                   }

                   new.value <- as.numeric(input$t_3_pop_size_text)
                   if( abs(new.value - old.value)>0.000001 )
                   {
                     updateSliderInput(session, "t_3_pop_size_slider", value = new.value)
                   }
                 }
               })

  observeEvent(input$t_3_pop_size_pop_sel,
               {
                 new.value <- 100
                 if(!is.null(input$t_3_pop_ref_sel) && length(app.variables$ref.objects)>0)
                 {
                   id <- as.numeric(input$t_3_pop_ref_sel)
                   if(!is.na(id) && id <= length(app.variables$ref.objects))
                   {
                     ref <- app.variables$ref.objects[[id]]
                     if(length(ref)>0)
                     {
                       ref.name <- ref$Name
                       if(length(app.variables$temp.objects)==0)
                       {
                         app.variables$temp.objects <- list()
                       }
                       if(is.null(app.variables$temp.objects[[ref.name]]))
                       {
                         app.variables$temp.objects[[ref.name]] <- ref

                         app.variables$temp.objects[[ref.name]]$OriginExpr <-
                           lapply(ref$FCSG2$Expr, function(mat){return(mat)})

                         app.variables$temp.objects[[ref.name]]$TempFrequencies <- rep(100, length(ref$FCSG2$Frequencies))
                       }
                       ref <- app.variables$temp.objects[[ref.name]]

                       pop <- as.numeric(input$t_3_pop_size_pop_sel)
                       if(length(ref$TempFrequencies)>0 && pop <= length(ref$TempFrequencies))
                       {
                         new.value <- ref$TempFrequencies[[pop]]
                       }
                     }
                   }
                 }
                 updateNumericInput(session,"t_3_pop_size_text", value=new.value)
                 updateSliderInput(session,"t_3_pop_size_slider", value=new.value)
               })

  observeEvent(input$t_3_pop_manage_rm, #DELETE POP
               {
                 if(!is.null(input$t_3_pop_ref_sel) && !is.null(input$t_3_pop_ref_sel))
                 {
                   id <- as.numeric(input$t_3_pop_ref_sel)
                   ref <- app.variables$ref.objects[[id]]
                   ref.name <- ref$Name
                   if(length(app.variables$temp.objects)>0 && !is.null(app.variables$temp.objects[[ref.name]]))
                   {
                     ref <- app.variables$temp.objects[[ref.name]]
                   }

                   if(length(ref)>0)
                   {
                     pop <- as.integer(input$t_3_pop_manage_rm_sel)

                     tmp.obj <- delete.population(ref$FCSG2, pop)

                     if(length(app.variables$temp.objects)==0)
                     {
                       app.variables$temp.objects <- list()
                     }
                     if(is.null(app.variables$temp.objects[[ref.name]]))
                     {
                       app.variables$temp.objects[[ref.name]] <- app.variables$ref.objects[[id]]

                       app.variables$temp.objects[[ref.name]]$OriginExpr <-
                         lapply(app.variables$ref.objects[[id]]$FCSG2$Expr, function(mat){return(mat)})

                       app.variables$temp.objects[[ref.name]]$TempFrequencies <- rep(100, length(ref$FCSG2$Frequencies))
                     }

                     app.variables$temp.objects[[ref.name]]$FCSG2 <- tmp.obj
                     app.variables$temp.objects[[ref.name]]$OriginExpr <- app.variables$temp.objects[[ref.name]]$OriginExpr[-pop]
                     app.variables$temp.objects[[ref.name]]$TempFrequencies <- app.variables$temp.objects[[ref.name]]$TempFrequencies[-pop]
                   }
                 }
               })

  observeEvent(input$t_3_pop_manage_add, #ADD POP
               {
                 if(!is.null(input$t_3_pop_ref_sel) && !is.null(input$t_3_pop_manage_add_sel))
                 {
                   id <- as.numeric(input$t_3_pop_ref_sel)
                   ref <- app.variables$ref.objects[[id]]
                   ref.name <- ref$Name
                   if(length(app.variables$temp.objects)>0 && !is.null(app.variables$temp.objects[[ref.name]]))
                   {
                     ref <- app.variables$temp.objects[[ref.name]]
                   }

                   if(length(ref)>0)
                   {
                     annotation.column <- ref$AnnotationColumn
                     markers <- ref$Markers

                     ref.population <- as.numeric(input$t_3_pop_manage_add_sel)
                     nmb.events <- as.numeric(input$t_3_pop_manage_add_events)

                     tmp.obj <- add.population(ref.object = ref$FCSG2, nmb.events = nmb.events,
                                               annotation.column = annotation.column,
                                               marker.columns = markers,
                                               ref.pop = ref.population,
                                               min.val = -0.5, max.val = 4.5)


                     if(length(app.variables$temp.objects)==0)
                     {
                       app.variables$temp.objects <- list()
                     }
                     if(is.null(app.variables$temp.objects[[ref.name]]))
                     {
                       app.variables$temp.objects[[ref.name]] <- app.variables$ref.objects[[id]]

                       app.variables$temp.objects[[ref.name]]$OriginExpr <-
                         lapply(app.variables$ref.objects[[id]]$FCSG2$Expr, function(mat){return(mat)})

                       app.variables$temp.objects[[ref.name]]$TempFrequencies <- rep(100, length(ref$FCSG2$Frequencies))
                     }
                     app.variables$temp.objects[[ref.name]]$FCSG2 <- tmp.obj

                     if(is.null(app.variables$temp.objects[[ref.name]]))
                     {
                       app.variables$temp.objects[[ref.name]] <- app.variables$ref.objects[[id]]
                     }

                     app.variables$temp.objects[[ref.name]]$FCSG2 <- tmp.obj
                     app.variables$temp.objects[[ref.name]]$OriginExpr[[length(app.variables$temp.objects[[ref.name]]$OriginExpr)+1]] <-
                       tmp.obj$Expr[[ref.population]]
                     app.variables$temp.objects[[ref.name]]$TempFrequencies[[length(app.variables$temp.objects[[ref.name]]$TempFreqiencies)+1]] <- 100
                   }
                 }
               })

  #UPDATE POP
  observe(
    {
      update.populations.from.temp("t_3_pop_ref_sel", "t_3_pop_manage_rm_sel")
      update.populations.from.temp("t_3_pop_ref_sel", "t_3_pop_manage_add_sel")
      update.populations.from.temp("t_3_pop_ref_sel", "t_3_pop_move_pop_sel")
      update.populations.from.temp("t_3_pop_ref_sel", "t_3_pop_size_pop_sel")
      update.populations.from.temp("t_3_pop_ref_sel", "t_3_pop_hp")
    })

  observeEvent(input$t_3_pop_generate, #GENERATE THE MODEL
               {
                 shinyjs::disable("t_3_pop_generate")
                 if(!is.null(input$t_3_pop_ref_sel))
                 {
                   id <- as.numeric(input$t_3_pop_ref_sel)
                   ref <- app.variables$ref.objects[[id]]
                   if(length(ref)>0)
                   {
                     progress <- Progress$new()
                     progress$set(message = "Validating modifications", value = 0)
                     ref.name <- ref$Name
                     if(length(ref)>0 && length(app.variables$temp.objects)>0 && ref.name%in%names(app.variables$temp.objects))
                     {
                       ref <- app.variables$temp.objects[[ref.name]]
                       annotation.column <- ref$AnnotationColumn

                       tmp.id <- which(names(app.variables$temp.objects)==ref.name)
                       if(length(tmp.id)>0)
                       {
                         tmp.name <- paste0(input$t_3_pop_model_name, "_", ref.name)
                         ref$Name <- tmp.name
                         ref$GroupSize <- 1
                         app.variables$ref.objects[[length(app.variables$ref.objects)+1]] <- ref
                         app.variables$temp.objects <- app.variables$temp.objects[-unlist(tmp.id)]
                       }
                     }
                     progress$set(message = "Model Generated", value = 1)
                     delay(500, progress$close())
                   }
                 }
                 delay(200, shinyjs::enable("t_3_pop_generate"))
               })
  #===================================================================







  #3.2 - Generate Model (PSEUDO-TIMELINE)
  #===================================================================
  observeEvent(input$t_3_tp_remove, #Remove button
               {
                 shinyjs::disable("t_3_tp_remove")

                 if( !is.null(input$t_3_tp_ref_sel) && !is.null(input$t_3_tp_remove_sel))
                 {
                   id <- as.numeric(input$t_3_tp_ref_sel)
                   ref <- app.variables$ref.objects[[id]]
                   if(length(ref)>0)
                   {
                     progress <- Progress$new()
                     progress$set(message = "Removing Files", value = 0)
                     ref.name <- ref$Name
                     obj.ids <- as.integer(input$t_3_tp_remove_sel)

                     app.variables$TP.objects[[ref.name]] <- app.variables$TP.objects[[ref.name]][-obj.ids]
                     app.variables$TP.list[[ref.name]] <- app.variables$TP.list[[ref.name]][-obj.ids]
                     progress$set(message = "Files Removed", value = 1)
                     delay(500, progress$close())
                   }
                 }

                 delay(500, shinyjs::enable("t_3_tp_remove"))
               })

  observe(
    {
      obj.list.ids <- list()
      if( !is.null(input$t_3_tp_ref_sel) )
      {
        id <- as.numeric(input$t_3_tp_ref_sel)
        if(!is.na(id) && id<=length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]
          ref.name <- ref$Name

          if(length(ref)>0)
          {
            if(length(app.variables$TP.objects[[ref.name]])>0)
            {
              obj.list.ids <- 1:length(app.variables$TP.objects[[ref.name]])
              TP.obj.names <- NULL
              if(length(app.variables$TP.objects[[ref.name]])>0)
              {
                TP.obj.names <- sapply(1:length(app.variables$TP.objects[[ref.name]]), function(i)
                {
                  return(app.variables$TP.objects[[ref.name]][[i]]$Name)
                })
              }
              names(obj.list.ids) <- TP.obj.names
            }
          }
        }
      }
      updateSelectInput(session, "t_3_tp_remove_sel", choices=obj.list.ids, selected = obj.list.ids)
    })

  observeEvent(input$t_3_tp_add,
               {
                 shinyjs::disable("t_3_tp_add")
                 if(!is.null(input$t_3_tp_ref_sel))
                 {
                   id <- as.numeric(input$t_3_tp_ref_sel)
                   ref <- app.variables$ref.objects[[id]]
                   if(length(ref)>0)
                   {
                     progress <- Progress$new()
                     progress$set(message = "Adding Time Point", value = 0)
                     ref.name <- ref$Name

                     markers <- ref$Markers
                     annotation.column <- ref$AnnotationColumn
                     comp <- ref$Compensated
                     transf <- ref$Transformed
                     nmb.pop <- length(ref$FCSG2$Expr)

                     pop.reduction.percentages <- rep(0, nmb.pop)
                     locked.populations = NULL

                     tmp.name <- paste0(ref$Name, "_TP_",length(app.variables$TP.list[[ref.name]]))


                     if(length(app.variables$TP.list)==0)
                     {
                       app.variables$TP.list <- list()
                     }

                     if(length(app.variables$TP.list[[ref.name]])==0)
                     {
                       app.variables$TP.list[[ref.name]] <- list()
                       app.variables$TP.list[[ref.name]][[1]] <- list("Populations" = 1:nmb.pop,
                                                                      "LockedPopulations" = locked.populations,
                                                                      "ReductionValues" = pop.reduction.percentages)
                     }
                     else
                     {
                       last.tp <- app.variables$TP.list[[ref.name]][[length(app.variables$TP.list[[ref.name]])]]
                       app.variables$TP.list[[ref.name]][[length(app.variables$TP.list[[ref.name]])+1]] <- last.tp
                       pop.reduction.percentages <- last.tp$ReductionValues
                       locked.populations <- last.tp$LockedPopulations
                       ref <- app.variables$TP.objects[[ref.name]][[length(app.variables$TP.objects[[ref.name]])]]
                     }

                     x <- generate.FCSG2.file.by.reducing.populations.in.ref(ref.object = ref$FCSG2,
                                                                             pop.reduction.percentages = pop.reduction.percentages,
                                                                             locked.populations = locked.populations,
                                                                             marker.columns = markers,
                                                                             annotation.column = annotation.column,
                                                                             min.val = -0.5, max.val = 4.5)

                     tmp.obj <- list("FCSG2"=x,
                                     "Transformed"=transf,
                                     "Compensated"=comp,
                                     "Name"=tmp.name,
                                     "AnnotationColumn"=annotation.column,
                                     "Markers"=markers )


                     if(length(app.variables$TP.objects)==0)
                     {
                       app.variables$TP.objects <- list()
                     }
                     if(length(app.variables$TP.objects[[ref.name]])==0)
                     {
                       app.variables$TP.objects[[ref.name]] <- list()
                     }
                     app.variables$TP.objects[[ref.name]][[length(app.variables$TP.objects[[ref.name]])+1]] <- tmp.obj
                     progress$set(message = "Time Point added (an update might be required)", value = 1)
                     delay(500, progress$close())
                   }
                 }
                 delay(300, shinyjs::enable("t_3_tp_add"))
               })

  observeEvent(input$t_3_tp_update,
               {
                 shinyjs::disable("t_3_tp_update")
                 if(!is.null(input$t_3_tp_ref_sel))
                 {
                   id <- as.numeric(input$t_3_tp_ref_sel)
                   ref <- app.variables$ref.objects[[id]]
                   if(length(ref)>0)
                   {
                     progress <- Progress$new()
                     progress$set(message = "Updating the Pseudo-Time", value = 0)
                     ref.name <- ref$Name
                     markers <- ref$Markers
                     annotation.column <- ref$AnnotationColumn
                     comp <- ref$Compensated
                     transf <- ref$Transformed

                     if(length(app.variables$TP.list[[ref.name]])>0)
                     {
                       for(i in 1:length(app.variables$TP.list[[ref.name]]))
                       {
                         tp <- app.variables$TP.list[[ref.name]][[i]]
                         if(i>1)
                         {
                           ref <- app.variables$TP.objects[[ref.name]][[i-1]]
                         }

                         red.pop <- sapply(1:length(tp$Populations), function(j)
                         {
                           return( as.numeric(input[[paste0("tp_", i, "_red_pop_", j)]]) )
                         })

                         locked.pop <- which(sapply(1:length(tp$Populations), function(j)
                         {
                           return( as.logical(input[[paste0("tp_", i, "_cb_pop_", j)]]) )
                         }))
                         if(length(locked.pop)==0)
                         {
                           locked.pop <- NULL
                         }

                         tmp.name <- app.variables$TP.objects[[ref.name]][[i]]$Name
                         x <- generate.FCSG2.file.by.reducing.populations.in.ref(ref.object = ref$FCSG2,
                                                                                 pop.reduction.percentages = red.pop,
                                                                                 locked.populations = locked.pop,
                                                                                 marker.columns = markers,
                                                                                 annotation.column = annotation.column,
                                                                                 min.val = -0.5, max.val = 4.5)

                         tmp.obj <- list("FCSG2"=x,
                                         "Transformed"=transf,
                                         "Compensated"=comp,
                                         "Name"=tmp.name,
                                         "AnnotationColumn"=annotation.column,
                                         "Markers"=markers )

                         app.variables$TP.list[[ref.name]][[i]]$LockedPopulations <- locked.pop
                         app.variables$TP.list[[ref.name]][[i]]$ReductionValues <- red.pop
                         app.variables$TP.objects[[ref.name]][[i]] <- tmp.obj

                       }
                     }
                     progress$set(message = "Pseudo-time updated", value = 1)
                     delay(500, progress$close())
                   }
                 }
                 delay(300, shinyjs::enable("t_3_tp_update"))
               })

  observeEvent(input$t_3_tp_generate, #Validate
               {
                 shinyjs::disable("t_3_tp_generate")
                 if( !is.null(input$t_3_tp_ref_sel) )
                 {
                   id <- as.numeric(input$t_3_tp_ref_sel)
                   if(!is.na(id) && id<=length(app.variables$ref.objects))
                   {
                     ref <- app.variables$ref.objects[[id]]

                     if(length(ref)>0)
                     {
                       ref.name <- ref$Name
                       if(length(app.variables$TP.objects)>0 && length(app.variables$TP.objects[[ref.name]])>0)
                       {
                         tp.ids <- 1:length(app.variables$TP.objects[[ref.name]])
                         nmb.files <- length(tp.ids)

                         if(length(tp.ids)>0 && !is.null(tp.ids))
                         {
                           progress <- Progress$new()
                           progress$set(message = "Generating the Models", value = 0)
                           for(i in tp.ids)
                           {
                             if(length(app.variables$ref.objects)==0)
                             {
                               app.variables$ref.objects <- list()
                             }
                             app.variables$ref.objects[[length(app.variables$ref.objects)+1]] <- app.variables$TP.objects[[ref.name]][[i]]
                             app.variables$ref.objects[[length(app.variables$ref.objects)]]$GroupSize <- 1
                             progress$inc(1/nmb.files, detail = paste(app.variables$TP.objects[[ref.name]][[i]]$Name, "Generated"))
                           }
                           app.variables$TP.objects[[ref.name]] <- app.variables$shift.objects[[ref.name]][-tp.ids]
                           app.variables$TP.list[[ref.name]] <- app.variables$TP.list[[ref.name]][-tp.ids]
                         }
                         progress$set(message = "Models Generated", value = 1)
                         delay(500, progress$close())
                       }
                     }
                   }
                 }
                 delay(200, shinyjs::enable("t_3_tp_generate"))
               })

  output$t_3_tp_body <- renderUI( #PLOT TIME POINTS
    {
      tp.ui <- NULL

      if(!is.null(input$t_3_tp_ref_sel))
      {
        id <- as.numeric(input$t_3_tp_ref_sel)
        ref <- app.variables$ref.objects[[id]]
        ref.name <- ref$Name
        markers <- ref$Markers
        annotation.column <- ref$AnnotationColumn
        comp <- ref$Compensated
        transf <- ref$Transformed

        if(length(ref)>0)
        {
          if(length(app.variables$TP.list[[ref.name]])>0)
          {
            tp.ui <- lapply(1:length(app.variables$TP.list[[ref.name]]), function(i)
            {
              tp <- app.variables$TP.list[[ref.name]][[i]]

              red.pop.ui <- lapply(1:length(tp$Populations), function(j)
              {
                tmp.ui <- tagList(
                  column(
                    width=12,
                    numericInput(paste0("tp_", i, "_red_pop_", j), tp$Populations[[j]], value=tp$ReductionValues[[j]])
                  )
                )

                return(tmp.ui)
              })

              locked.pop.ui <- lapply(1:length(tp$Populations), function(j)
              {
                l <- F
                if(j%in%unlist(tp$LockedPopulations))
                {
                  l <- T
                }
                tmp.ui <- tagList(
                  column(
                    width=12,
                    checkboxInput(paste0("tp_", i, "_cb_pop_", j), tp$Populations[[j]], value=l)
                  )
                )

                return(tmp.ui)
              })

              collapsed <- T
              if(i==length(app.variables$TP.list[[ref.name]]))
              {
                collapsed <- F
              }

              tmp.ui <- tagList(
                shinydashboard::box(
                  width=12, solidHeader=T, status="info", collapsible = T, collapsed = collapsed,
                  title=paste("Pseudo-Time - Time Point", i),

                  column(
                    width=4,
                    shinydashboard::box(
                      width=12,title="Reduction %", style="overflow:auto;max-height:17vh",
                      solidHeader=T,status="info",
                      red.pop.ui
                    ),
                    shinydashboard::box(
                      width=12,title="Locked Populations", style="overflow:auto;max-height:17vh",
                      solidHeader=T,status="info",
                      locked.pop.ui
                    )
                  ),
                  column(
                    width=8,
                    plotOutput(paste0("tp_",i,"_plot"))
                  )
                )
              )

              output[[paste0("tp_",i,"_plot")]] <- renderPlot(
                {
                  tmp.plot <- NULL

                  if(!is.null(input$t_3_tp_M1) && !is.null(input$t_3_tp_M2)
                     && length(app.variables$TP.objects[[ref.name]])>0)
                  {
                    m1 <- as.integer(input$t_3_tp_M1)
                    m2 <- as.integer(input$t_3_tp_M2)
                    if(!m1==m2)
                    {
                      locked.pop <- tp$LockedPopulations
                      mod.pop <- which(tp$ReductionValues>0)
                      free.pop <- (1:length(tp$Populations))[-unique(c(locked.pop,mod.pop))]

                      x <- app.variables$TP.objects[[ref.name]][[i]]$FCSG2
                      mat <- NULL
                      pop.type <-NULL
                      for(i in 1:length(x$Expr))
                      {
                        if(i%in%mod.pop)
                        {
                          pop.type <- c(pop.type, rep("Reduced",nrow(x$Expr[[i]])))
                        }
                        else if(i%in%free.pop)
                        {
                          pop.type <- c(pop.type, rep("Free",nrow(x$Expr[[i]])))
                        }
                        else
                        {
                          pop.type <- c(pop.type, rep("Locked",nrow(x$Expr[[i]])))
                        }
                        mat <- rbind(mat,
                                     x$Expr[[i]])
                      }
                      df <- as.data.frame(mat[,c(m1,m2)])
                      df <- cbind(df,
                                  "Type"=pop.type)
                      df$Type <- as.factor(df$Type)
                      tmp.plot <- ggplot(df, aes(df[,1],df[,2])) + geom_point(aes(colour=Type))
                    }
                  }

                  return(tmp.plot)
                })

              return(tmp.ui)
            })
          }
        }
      }

      return(tp.ui)
    })
  #===================================================================






  #3.3 - Generate Model (MIX FILES)
  #===================================================================
  observe( #MARKERS
    {
      markers <- list(NULL=NULL)
      m1 <- list(NULL=NULL)
      m2 <- list(NULL=NULL)
      if(!is.null(input$t_3_mix_r1_sel) && !is.null(input$t_3_mix_r2_sel))
      {
        id1 <- as.numeric(input$t_3_mix_r1_sel)
        ref1 <- NULL
        ref1.name <- NULL
        if(!is.na(id1) && id1<=length(app.variables$ref.objects))
        {
          ref1 <- app.variables$ref.objects[[id1]]
          ref1.name <- ref1$Name
        }

        id2 <- as.numeric(input$t_3_mix_r2_sel)
        ref2 <- NULL
        ref2.name <- NULL
        if(!is.na(id2) && id2<=length(app.variables$ref.objects))
        {
          ref2 <- app.variables$ref.objects[[id2]]
          ref2.name <- ref2$Name
        }

        if(length(ref1)>0 && length(ref2)>0)
        {
          markers.names.r1 <- colnames(ref1$FCSG2$Expr[[1]])[ref1$Markers]
          markers.names.r2 <- colnames(ref2$FCSG2$Expr[[1]])[ref2$Markers]

          markers <- unique(intersect(markers.names.r1, markers.names.r2))
          names(markers) <- markers
          if(length(markers)>0)
          {
            m1 <- markers[[1]]
          }
          if(length(markers)>1)
          {
            m2 <- markers[[2]]
          }
        }
      }
      updateSelectInput(session, "t_3_mix_m1", choices=markers, selected = m1)
      updateSelectInput(session, "t_3_mix_m2", choices=markers, selected = m2)
    })

  observeEvent(input$t_3_mix_generate, #GENERATE THE FILE
               {
                 shinyjs::disable("t_3_mix_generate")
                 if(!is.null(input$t_3_mix_r1_sel) && !is.null(input$t_3_mix_r2_sel) && !is.null(input$t_3_mix_r1_p) && !is.null(input$t_3_mix_r2_p))
                 {
                   id.1 <- as.numeric(input$t_3_mix_r1_sel)
                   ref.1 <- app.variables$ref.objects[[id.1]]
                   ref.1.name <- ref.1$Name
                   populations.1 <- as.numeric(input$t_3_mix_r1_pop_sel)

                   id.2 <- as.numeric(input$t_3_mix_r2_sel)
                   ref.2 <- app.variables$ref.objects[[id.2]]
                   ref.2.name <- ref.2$Name
                   populations.2 <- as.numeric(input$t_3_mix_r2_pop_sel)

                   t1 <- as.numeric(input$t_3_mix_r1_p)
                   t2 <- as.numeric(input$t_3_mix_r2_p)

                   if(length(ref.1)>0 && length(ref.2)>0 && !is.null(t1) && !is.null(t2))
                   {
                     progress <- Progress$new()
                     progress$set(message = "Merging Reference Files", value = 0)
                     markers.names.r1 <- colnames(ref.1$FCSG2$Expr[[1]])[ref.1$Markers]
                     markers.names.r2 <- colnames(ref.2$FCSG2$Expr[[1]])[ref.2$Markers]

                     markers <- unique(intersect(markers.names.r1, markers.names.r2))
                     markers.ids.1 <- NULL
                     markers.ids.2 <- NULL
                     for(i in 1:length(markers))
                     {
                       m.id <- which(markers.names.r1==markers[[i]])
                       if(length(m.id)>0)
                       {
                         markers.ids.1 <- c(markers.ids.1, as.integer(m.id[[1]]))
                       }

                       m.id <- which(markers.names.r2==markers[[i]])
                       if(length(m.id)>0)
                       {
                         markers.ids.2 <- c(markers.ids.2, as.integer(m.id[[1]]))
                       }
                     }

                     x1 <- ref.1$FCSG2$Expr
                     x2 <- ref.2$FCSG2$Expr
                     x <- mix.files(ref.obj1 = ref.1$FCSG2, ref.obj2 = ref.2$FCSG2, t1, t2,
                                    annotation.column.1 = ref.1$AnnotationColumn, annotation.column.2 = ref.2$AnnotationColumn,
                                    markers.1 = markers.ids.1, markers.2 = markers.ids.2)

                     tmp.name <- paste0("MIX_",id.1,"_",id.2,
                                        "__",ref.1.name,"__",ref.2.name)
                     if(length(app.variables$mix.objects)==0)
                     {
                       app.variables$mix.objects <- list()
                     }
                     app.variables$mix.objects <- list.append(app.variables$mix.objects,
                                                              as.list(list("FCSG2"=x,
                                                                           "Transformed"=T,
                                                                           "Compensated"=T,
                                                                           "Name"=tmp.name,
                                                                           "AnnotationColumn"=length(markers)+1,
                                                                           "Markers"=1:length(markers),
                                                                           "GroupSize"=1)))
                     names(app.variables$mix.objects)[[length(app.variables$mix.objects)]] <- tmp.name
                     progress$set(message = "File Generated", value = 1)
                     delay(500, progress$close())
                   }
                 }
                 delay(300, shinyjs::enable("t_3_mix_generate"))
               })

  observe( #NEW POPULATIONS
    {
      populations <- list(NULL=NULL)
      if(!is.null(input$t_3_mix_r1_sel) && !is.null(input$t_3_mix_r2_sel))
      {
        id.1 <- as.numeric(input$t_3_mix_r1_sel)
        ref.1 <- NULL
        ref.1.name <- NULL
        if(!is.na(id.1) && id.1<=length(app.variables$ref.objects))
        {
          ref.1 <- app.variables$ref.objects[[id.1]]
          ref.1.name <- ref.1$Name
        }

        id.2 <- as.numeric(input$t_3_mix_r2_sel)
        ref.2 <- NULL
        ref.2.name <- NULL
        if(!is.na(id.2) && id.2<=length(app.variables$ref.objects))
        {
          ref.2 <- app.variables$ref.objects[[id.2]]
          ref.2.name <- ref.2$Name
        }

        if(length(ref.1)>0 && length(ref.2)>0)
        {

          tmp.name <- paste0("MIX_",id.1,"_",id.2,
                             "__",ref.1.name,"__",ref.2.name)

          if(length(app.variables$mix.objects)>0 && !is.null(app.variables$mix.objects[[tmp.name]]))
          {
            ref <- app.variables$mix.objects[[tmp.name]]
            populations <- 1:length(ref$FCSG2$Expr)
          }
        }
      }
      updateSelectInput(session, "t_3_mix_hp", choices=populations, selected = populations)
    })

  output$t_3_mix_plot <- renderPlot( #VISUALIZATION
    {
      viz.plot <- NULL

      m1 <- input[["t_3_mix_m1"]]
      m2 <- input[["t_3_mix_m2"]]
      if(!is.null(input$t_3_mix_r1_sel) && !is.null(input$t_3_mix_r2_sel) && !is.null(m1) && !is.null(m2))
      {
        id.1 <- as.numeric(input$t_3_mix_r1_sel)
        ref.1 <- app.variables$ref.objects[[id.1]]
        ref.1.name <- ref.1$Name
        populations.1 <- as.numeric(input$t_3_mix_r1_pop_sel)

        id.2 <- as.numeric(input$t_3_mix_r2_sel)
        ref.2 <- app.variables$ref.objects[[id.2]]
        ref.2.name <- ref.2$Name
        populations.2 <- as.numeric(input$t_3_mix_r2_pop_sel)

        tmp.name <- paste0("MIX_",id.1,"_",id.2,
                           "__",ref.1.name,"__",ref.2.name)

        if(length(app.variables$mix.objects)>0 && !is.null(app.variables$mix.objects[[tmp.name]]))
        {
          ref <- app.variables$mix.objects[[tmp.name]]
          markers <- ref$Markers
          highlighted.population <- as.numeric(input$t_3_mix_hp)

          if(length(ref)>0)
          {
            x <- ref$FCSG2
            mat <- NULL
            pop.type <-NULL
            for(i in 1:length(x$Expr))
            {
              if(i%in%highlighted.population)
              {
                pop.type <- c(pop.type, rep("Highlighted", nrow(x$Expr[[i]])))
              }
              else
              {
                pop.type <- c(pop.type, rep("Others", nrow(x$Expr[[i]])))
              }
              mat <- rbind(mat, x$Expr[[i]])
            }


            m1 <- as.integer(markers[which(colnames(mat)==m1)][[1]])
            m2 <- as.integer(markers[which(colnames(mat)==m2)][[1]])


            df <- as.data.frame(mat)[,c(m1,m2)]
            colnames(df) <- colnames(x$Expr[[1]])[c(m1,m2)]
            df$Population <- as.factor(pop.type)
            pop.colors <- c(Highlighted="green", Others="black")

            viz.plot <- ggplot(df, aes(df[,1], df[,2], colour=Population)) +
              geom_point(size=0.8) + scale_colour_manual(values=pop.colors) + xlim(c(-0.5,4.5)) + ylim(c(-0.5,4.5))
          }
        }
      }

      return(viz.plot)
    })

  observeEvent(input$t_3_mix_validate,
               {
                 shinyjs::disable("t_3_mix_validate")
                 if(!is.null(input$t_3_mix_r1_sel) && !is.null(input$t_3_mix_r2_sel))
                 {
                   id.1 <- as.numeric(input$t_3_mix_r1_sel)
                   ref.1 <- app.variables$ref.objects[[id.1]]
                   ref.1.name <- ref.1$Name
                   populations.1 <- as.numeric(input$t_3_mix_r1_pop_sel)

                   id.2 <- as.numeric(input$t_3_mix_r2_sel)
                   ref.2 <- app.variables$ref.objects[[id.2]]
                   ref.2.name <- ref.2$Name
                   populations.2 <- as.numeric(input$t_3_mix_r2_pop_sel)

                   if(length(ref.1)>0 && length(ref.2)>0)
                   {
                     progress <- Progress$new()
                     progress$set(message = "Adding the file to references", value = 0)
                     tmp.name <- paste0("MIX_",id.1,"_",id.2,
                                        "__",ref.1.name,"__",ref.2.name)
                     if(length(app.variables$mix.objects)>0 && !is.null(app.variables$mix.objects[[tmp.name]]))
                     {
                       app.variables$ref.objects[[length(app.variables$ref.objects)+1]] <- app.variables$mix.objects[[tmp.name]]

                       mix.id <- which(names(app.variables$mix.objects)==tmp.name)[[1]]
                       app.variables$mix.objects <- app.variables$mix.objects[-mix.id]

                     }
                     progress$set(message = "File added", value = 1)
                     delay(500, progress$close())
                   }
                 }
                 delay(300, shinyjs::enable("t_3_mix_validate"))
               })
  #===================================================================







  #4 - GENERATE GROUPS FROM A MODEL
  #===================================================================
  observe( #REF HEATMAP
    {
      ref.hm.plot <- NULL

      id <- as.numeric(input$t_4_ref_sel)
      if(!is.na(id) && id <= length(app.variables$ref.objects))
      {
        ref <- app.variables$ref.objects[[id]]
        ref.name <- ref$Name
        markers <- ref$Markers
        annotation.column <- ref$AnnotationColumn
        comp <- ref$Compensated
        transf <- ref$Transformed

        if(length(ref)>0)
        {
          x <- ref$FCSG2
          ref.pop.mat <- matrix(nrow=length(x$Expr), ncol=length(markers))
          colnames(ref.pop.mat) <- colnames(x$Expr[[1]])[markers]
          rownames(ref.pop.mat) <- rep("", nrow(ref.pop.mat))
          for(k in 1:length(x$Expr))
          {
            mat <- x$Expr[[k]]
            rownames(ref.pop.mat)[k] <- mat[1,annotation.column]
            for(j in markers)
            {
              ref.pop.mat[k,j] <- mean(mat[,j])
            }
          }
          ref.hm.plot <- heatmaply(ref.pop.mat, Rowv = T, Colv="Rowv", dendrogram = "none", limits = c(-0.5,4.5))
        }
      }
      output[[paste0("t_4_hm_ref")]] <- renderPlotly(ref.hm.plot)
    })

  output$t_4_hm_list <- renderUI( #PLOT HEATMAPS
    {
      hm.ui <- NULL

      if(!is.null(input$t_4_ref_sel))
      {
        id <- as.numeric(input$t_4_ref_sel)
        if(!is.na(id) && id<=length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]
          ref.name <- ref$Name
          markers <- ref$Markers
          annotation.column <- ref$AnnotationColumn
          comp <- ref$Compensated
          transf <- ref$Transformed

          if(length(ref)>0)
          {
            if(length(app.variables$group.objects[[ref.name]])>0)
            {
              hm.ui <- lapply(1:length(app.variables$group.objects[[ref.name]]), function(i)
              {
                tmp.ref <- app.variables$group.objects[[ref.name]][[i]]
                x <- app.variables$group.objects[[ref.name]][[i]]$FCSG2

                tmp.id <- i
                tmp.ui <- tagList(
                  shinydashboard::box(
                    width = 12,
                    collapsible = T, collapsed = T,
                    title=tmp.ref$Name,
                    solidHeader=T,status="info",style="overflow:auto",
                    plotlyOutput(paste0("t_4_hm_list_",tmp.id,"_plot"), width = "100%")
                  )
                )

                pop.mat <- matrix(nrow=length(x$Expr), ncol=length(markers))
                colnames(pop.mat) <- colnames(x$Expr[[1]])[markers]
                rownames(pop.mat) <- rep("", nrow(pop.mat))
                for(k in 1:length(x$Expr))
                {
                  mat <- x$Expr[[k]]
                  rownames(pop.mat)[k] <- mat[1,annotation.column]
                  for(j in markers)
                  {
                    pop.mat[k,j] <- mean(mat[,j])
                  }
                }
                pop.mat <- isolate(pop.mat)
                pop.plot <- heatmaply(pop.mat, Rowv = T, Colv="Rowv", dendrogram = "none", limits = c(-0.5,4.5))
                output[[paste0("t_4_hm_list_",tmp.id,"_plot")]] <- renderPlotly(isolate(pop.plot))

                return(tmp.ui)
              })
            }
          }
        }
      }

      return(hm.ui)
    })

  observeEvent(input$t_4_generate,
               {
                 shinyjs::disable("t_4_generate")
                 if(!is.null(input$t_4_ref_sel))
                 {
                   progress <- Progress$new()
                   progress$set(message = "Generating the Files", value = 0)

                   id <- as.numeric(input$t_4_ref_sel)
                   ref <- app.variables$ref.objects[[id]]


                   if(length(ref)>0)
                   {
                     ref.name <- ref$Name

                     if( length(app.variables$group.objects)>0 && (ref.name%in%names(app.variables$group.objects)) )
                     {
                       app.variables$group.objects[[ref.name]] <- NULL
                     }

                     shifted.markers <- as.numeric(input$t_4_shifted_markers)

                     nmb.files <- as.numeric(input$t_4_nmb_files)
                     m.shift <- as.numeric(input$t_4_mean_shift)
                     sd.shift <- as.numeric(input$t_4_sd_shift)

                     markers <- ref$Markers
                     annotation.column <- ref$AnnotationColumn
                     comp <- ref$Compensated
                     transf <- ref$Transformed

                     fcsg2.list <- generate.FCSG2.files.from.reference(ref.object = ref$FCSG2, nmb.files = nmb.files,
                                                                       markers.to.change = shifted.markers, markers.to.invert = NULL,
                                                                       m.shift = m.shift, sd.shift = sd.shift)

                     app.variables$ref.objects[[id]]$GroupSize <- app.variables$ref.objects[[id]]$GroupSize+nmb.files

                     tmp.obj.list <- lapply(1:length(fcsg2.list), function(i)
                     {
                       x <- fcsg2.list[[i]]
                       tmp.name <- paste0(i, "_", ref$Name)

                       tmp.obj <- list("FCSG2"=x,
                                       "Transformed"=transf,
                                       "Compensated"=comp,
                                       "Name"=tmp.name,
                                       "AnnotationColumn"=annotation.column,
                                       "Markers"=markers )
                       return(tmp.obj)
                     })


                     if(length(app.variables$group.objects)==0)
                     {
                       app.variables$group.objects <- list()
                     }
                     if( !(ref.name%in%names(app.variables$group.objects)) )
                     {
                       app.variables$group.objects[[ref.name]] <- list()
                     }
                     for(tmp.obj in tmp.obj.list)
                     {
                       app.variables$group.objects[[ref.name]][[length(app.variables$group.objects[[ref.name]])+1]] <- tmp.obj
                     }

                     progress$set(message = "Files Generated", value = 1)
                   }

                   delay(500, progress$close())
                 }
                 delay(500, shinyjs::enable("t_4_generate"))
               })
  #===================================================================







  #6.1 - HEATMAPS VISUALIZATIONS
  #===================================================================
  output$t_6_hm_sb_1 <- renderSunburst(
    {
      file.plot <- NULL
      if(length(app.variables$ref.objects)>0)
      {
        id <- as.integer(input$t_6_hm_ref1_sel)
        if(!is.null(id) && id <= length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]
          if(length(ref) > 0)
          {
            tmp.mat <- matrix(ncol = 2, nrow=length(ref$FCSG2$Expr))

            tmp.mat[,1] <- 1:nrow(tmp.mat)
            tmp.mat[,2] <- as.numeric(unlist(ref$FCSG2$Frequencies))
            colnames(tmp.mat) <- c("level1", "size")

            tmp.df <- data.frame(tmp.mat, stringsAsFactors = T)
            tmp.tree <- d3_nest(tmp.df, value_cols = "size")
            file.plot <- sunburst(tmp.tree, legend = TRUE)
          }
        }
      }
      return(file.plot)
    })

  output$t_6_hm_sb_2 <- renderSunburst(
    {
      file.plot <- NULL
      if(length(app.variables$ref.objects)>0)
      {
        id <- as.integer(input$t_6_hm_ref2_sel)
        if(!is.null(id) && id <= length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]
          if(length(ref) > 0)
          {
            tmp.mat <- matrix(ncol = 2, nrow=length(ref$FCSG2$Expr))

            tmp.mat[,1] <- 1:nrow(tmp.mat)
            tmp.mat[,2] <- as.numeric(unlist(ref$FCSG2$Frequencies))
            colnames(tmp.mat) <- c("level1", "size")

            tmp.df <- data.frame(tmp.mat, stringsAsFactors = T)
            tmp.tree <- d3_nest(tmp.df, value_cols = "size")
            file.plot <- sunburst(tmp.tree, legend = TRUE)
          }
        }
      }
      return(file.plot)
    })

  output$t_6_hm_1 <- renderPlotly(
    {
      hm.plot <- NULL
      if(length(app.variables$ref.objects)>0)
      {
        id <- as.integer(input$t_6_hm_ref1_sel)
        if(!is.null(id) && id <= length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]
          if(length(ref) > 0)
          {
            markers <- ref$Markers
            annotation.column <- ref$AnnotationColumn

            pop.mat <- matrix(nrow=length(ref$FCSG2$Expr), ncol=length(markers))
            colnames(pop.mat) <- colnames(ref$FCSG2$Expr[[1]])[markers]
            rownames(pop.mat) <- rep("", nrow(pop.mat))

            for(i in 1:length(ref$FCSG2$Expr))
            {
              mat <- ref$FCSG2$Expr[[i]]
              rownames(pop.mat)[i] <- mat[1,annotation.column]
              for(j in markers)
              {
                pop.mat[i,j] <- mean(mat[,j])
              }
            }
            hm.plot <- heatmaply(pop.mat, Rowv = T, Colv="Rowv", dendrogram = "none", limits = c(-0.5,4.5))
          }
        }
      }
      return(hm.plot)
    })

  output$t_6_hm_2 <- renderPlotly(
    {
      hm.plot <- NULL
      if(length(app.variables$ref.objects)>0)
      {
        id <- as.integer(input$t_6_hm_ref2_sel)
        if(!is.null(id) && id <= length(app.variables$ref.objects))
        {
          ref <- app.variables$ref.objects[[id]]
          if(length(ref) > 0)
          {
            markers <- ref$Markers
            annotation.column <- ref$AnnotationColumn

            pop.mat <- matrix(nrow=length(ref$FCSG2$Expr), ncol=length(markers))
            colnames(pop.mat) <- colnames(ref$FCSG2$Expr[[1]])[markers]
            rownames(pop.mat) <- rep("", nrow(pop.mat))

            for(i in 1:length(ref$FCSG2$Expr))
            {
              mat <- ref$FCSG2$Expr[[i]]
              rownames(pop.mat)[i] <- mat[1,annotation.column]
              for(j in markers)
              {
                pop.mat[i,j] <- mean(mat[,j])
              }
            }
            hm.plot <- heatmaply(pop.mat, Rowv = T, Colv="Rowv", dendrogram = "none", limits = c(-0.5,4.5))
          }
        }
      }
      return(hm.plot)
    })
  #===================================================================





  #6.2 - JOYPLOTS VISUALIZATIONS
  #===================================================================
  observe( #MARKERS
    {
      markers.list <- list(NULL=NULL)
      if(length(app.variables$ref.objects)>0)
      {
        ids <- as.integer(input$t_6_jp_ref_sel)
        if(!is.null(ids) && length(ids)>0)
        {
          markers.list <- NULL
          for(id in ids)
          {
            if(!is.na(id) && id <= length(app.variables$ref.objects))
            {
              ref <- app.variables$ref.objects[[id]]
              if(length(ref) > 0)
              {
                tmp.markers.names <- colnames(ref$FCSG2$Expr[[1]])[ref$Markers]
                if(is.null(markers.list))
                {
                  markers.list <- tmp.markers.names
                }
                else
                {
                  markers.list <- intersect(markers.list, tmp.markers.names)
                }
              }
            }
          }
          names(markers.list) <- markers.list
        }
      }
      updateSelectInput(session, "t_6_jp_marker_sel", choices = markers.list, selected = markers.list)
    })

  output$t_6_jp <- renderPlot(
    {
      jp.plot <- NULL
      if(length(app.variables$ref.objects)>0)
      {
        ids <- as.integer(input$t_6_jp_ref_sel)
        if(!is.null(ids) && length(ids)>0)
        {
          x <- NULL
          marker <- input$t_6_jp_marker_sel
          if(length(marker)>0 && !is.null(marker))
          {
            for(id in ids)
            {
              tmp.id <- NULL
              if(!is.na(id) && id <= length(app.variables$ref.objects))
              {
                ref <- app.variables$ref.objects[[id]]
                if(length(ref)>0)
                {
                  tmp.id <- which(colnames(ref$FCSG2$Expr[[1]])[ref$Markers] == marker)
                  if(length(tmp.id) > 0)
                  {
                    tmp.id <- tmp.id[[1]]
                  }

                  for(pop in 1:length(ref$FCSG2$Expr))
                  {
                    if(is.null(x))
                    {
                      x <- data.frame(par=ref$FCSG2$Expr[[pop]][,tmp.id],
                                      file=ref$Name)
                    }
                    else
                    {
                      x <- rbind(x, data.frame(par=ref$FCSG2$Expr[[pop]][,tmp.id],
                                               file=ref$Name))
                    }
                  }

                }
              }
            }
            if(!is.null(x))
            {
              x$file <- as.factor(x$file)
              jp.plot <- ggplot(x, aes(x=par,y=file)) + geom_density_ridges2() +
                xlab(marker) + ylab("density") + xlim(c(-0.5,4.5))
            }
          }
        }
      }

      return(jp.plot)
    })
  #===================================================================





  #6.3 - SCATTER PLOT VISUALIZATIONS
  #===================================================================
  output$t_6_sc <- renderPlot(
    {
      markers.plot <- NULL
      if(!is.null(input$t_6_sc_ref_sel))
      {
        m1 <- as.integer(input[["t_6_sc_m1_sel"]])
        m2 <- as.integer(input[["t_6_sc_m2_sel"]])

        id <- as.numeric(input$t_6_sc_ref_sel)
        ref <- app.variables$ref.objects[[id]]
        if(length(ref)>0 && !is.null(m1) && !is.null(m2))
        {
          ref.name <- ref$Name

          annotation.column <- ref$AnnotationColumn
          highlighted.population <- as.numeric(input$t_6_sc_hp)

          if( !(m1==m2) )
          {
            x <- ref$FCSG2

            mat <- NULL
            pop.type <- NULL
            for(i in 1:length(x$Expr))
            {
              if(i%in%highlighted.population)
              {
                pop.type <- c(pop.type, rep("Highlighted", nrow(x$Expr[[i]])))
              }
              else
              {
                pop.type <- c(pop.type, rep("Others", nrow(x$Expr[[i]])))
              }
              mat <- rbind(mat, x$Expr[[i]])
            }

            df <- as.data.frame(mat)[,c(m1,m2)]
            colnames(df) <- colnames(x$Expr[[1]])[c(m1,m2)]
            df$Population <- as.factor(pop.type)
            pop.colors <- c(Highlighted="green", Others="black")

            markers.plot <- ggplot(df, aes(df[,1], df[,2], colour=Population)) +
              geom_point(size=0.8) + scale_colour_manual(values=pop.colors) + xlim(c(-0.5,4.5)) + ylim(c(-0.5,4.5)) +
              xlab(colnames(df)[1]) + ylab(colnames(df)[2])
          }
        }
      }

      return(markers.plot)
    })
  #===================================================================







  #7 - Decompensate, Detransform
  #===================================================================
  output$t_7_files <- renderUI(
    {
      files.ui <- NULL
      if(length(app.variables$ref.objects)>0)
      {
        files.ui <- lapply(1:length(app.variables$ref.objects), function(i)
        {
          tmp.ui <- NULL

          ref <- app.variables$ref.objects[[i]]
          if(length(ref)>0)
          {
            comp <- p("False", style="color:red")
            if(ref$Compensated)
            {
              comp <- p("True", style="color:green")
            }
            transf <- p("False", style="color:red")
            if(ref$Transformed)
            {
              transf <- p("True", style="color:green")
            }

            tmp.ui <- tagList(
              fluidRow
              (
                style="margin-bottom:2%",
                column
                (
                  width=2,
                  checkboxInput(paste0("t_7_file_",i), "", value = F)
                ),
                column
                (
                  width=4,
                  h5(ref$Name)
                ),
                column
                (
                  width=3,
                  tags$b(comp)
                ),
                column
                (
                  width=3,
                  tags$b(transf)
                )
              )
            )
          }

          return(tmp.ui)
        })
      }

      return(files.ui)
    })

  output$t_7_detransform_param <- renderUI(
    {
      param.ui <- NULL
      if(!is.null(input$t_7_transform_sel) && as.integer(input$t_7_transform_sel) == 2)
      {
        param.ui <- numericInput("t_7_arcsinh_w", "W", value=5)
      }
      return(param.ui)
    })

  observeEvent(input$t_7_decompensate,
               {
                 shinyjs::disable("t_7_decompensate")
                 if(length(app.variables$ref.objects)>0)
                 {
                   progress <- Progress$new()
                   progress$set(message = "Decompensating Selection", value = 0)

                   selected.ref <- sapply(1:length(app.variables$ref.objects), function(i)
                   {
                     return(as.logical(input[[paste0("t_7_file_",i)]]))
                   })

                   selected.ref <- unlist(which(selected.ref))
                   if(length(selected.ref)>0)
                   {
                     for(i in selected.ref)
                     {
                       ref <- app.variables$ref.objects[[i]]
                       if(length(ref)>0)
                       {
                         tmp.fcs <- save.object.as.FCS(ref$FCSG2)
                         tmp.fcs <- m.inv.compensate(tmp.fcs)
                         tmp.obj <- load.annotated.FCS.as.object(fcs = tmp.fcs, annotation.column = ref$AnnotationColumn, markers.list = ref$Markers)

                         app.variables$ref.objects[[i]]$FCSG2 <- tmp.obj
                         app.variables$ref.objects[[i]]$Compensated <- F
                         progress$inc(1/length(selected.ref), detail=paste0(ref$Name, " decompensated"))
                       }
                     }
                   }
                   progress$set(message = "Files decompensated", value = 1)
                   shinyjs::delay(500, progress$close())
                 }
                 delay(500, shinyjs::enable("t_7_decompensate"))
               })

  observeEvent(input$t_7_detransform,
               {
                 shinyjs::disable("t_7_detransform")
                 if(length(app.variables$ref.objects)>0)
                 {
                   progress <- Progress$new()
                   progress$set(message = "Detransforming Selection", value = 0)
                   selected.ref <- sapply(1:length(app.variables$ref.objects), function(i)
                   {
                     return(as.logical(input[[paste0("t_7_file_",i)]]))
                   })

                   selected.ref <- unlist(which(selected.ref))
                   if(length(selected.ref)>0)
                   {
                     for(i in selected.ref)
                     {
                       ref <- app.variables$ref.objects[[i]]
                       if(length(ref)>0)
                       {
                         tmp.fcs <- save.object.as.FCS(ref$FCSG2)
                         if(!is.null(input$t_7_detransform_sel))
                         {
                           if(as.integer(input$t_7_detransform_sel) == 1)
                           {
                             tmp.fcs <- m.inv.transform.logicle(tmp.fcs, colnames(ref$FCSG2$Expr[[1]])[ref$Markers])
                           }
                           else if(as.integer(input$t_7_detransform_sel) == 2)
                           {
                             w <- as.numeric(input[["t_7_arcsinh_w"]])
                             tmp.fcs <- m.inv.transform.asinh(tmp.fcs, colnames(ref$FCSG2$Expr[[1]])[ref$Markers], w)
                           }
                         }
                         tmp.obj <- load.annotated.FCS.as.object(fcs = tmp.fcs, annotation.column = ref$AnnotationColumn, markers.list = ref$Markers)

                         app.variables$ref.objects[[i]]$FCSG2 <- tmp.obj
                         app.variables$ref.objects[[i]]$Transformed <- F
                         progress$inc(1/length(selected.ref), detail=paste0(ref$Name, " detransformed"))
                       }
                     }
                   }
                   progress$set(message = "Files Detransformed", value = 1)
                   shinyjs::delay(500, progress$close())
                 }
                 delay(500, shinyjs::enable("t_7_decompensate"))
               })

  observe( #SELECT ALL
    {
      if(length(app.variables$ref.objects)>0)
      {
        if(as.logical(input$t_7_file_select_all))
        {
          for(i in 1:length(app.variables$ref.objects))
          {
            updateCheckboxInput(session, paste0("t_7_file_",i), value = T)
          }
        }
        else
        {
          for(i in 1:length(app.variables$ref.objects))
          {
            updateCheckboxInput(session, paste0("t_7_file_",i), value = F)
          }
        }
      }
    })
  #===================================================================







  #8 - Download
  #===================================================================
  output$t_8_files <- renderUI(
    {
      files.ui <- NULL
      if(length(app.variables$ref.objects)>0)
      {
        files.ui <- lapply(1:length(app.variables$ref.objects), function(i)
        {
          tmp.ui <- NULL

          ref <- app.variables$ref.objects[[i]]
          if(length(ref)>0)
          {
            tmp.ui <- tagList(
              fluidRow
              (
                style="margin-top:2%",
                column
                (
                  width=1,
                  checkboxInput(paste0("t_8_ref_",i), "", value = F)
                ),
                column
                (
                  width=10,
                  h4(ref$Name)
                )
              )
            )

            if(length(app.variables$group.objects)>0 && ref$Name %in%names(app.variables$group.objects) &&
               length(app.variables$group.objects[[ref$Name]])>0)
            {
              tmp.ui <- list(tmp.ui,
                             lapply(1:length(app.variables$group.objects[[ref$Name]]), function(j)
                             {
                               tmp.obj <- app.variables$group.objects[[ref$Name]][[j]]
                               return(tagList(
                                 fluidRow
                                 (
                                   column
                                   (
                                     width=4
                                   ),
                                   column
                                   (
                                     width=8,
                                     h5(tmp.obj$Name)
                                   )
                                 )
                               ))

                             }))

            }
          }

          return(tmp.ui)
        })
      }

      return(files.ui)
    })

  observeEvent(input$t_8_select_all,
               {
                 if(length(app.variables$ref.objects)>0)
                 {
                   for(i in 1:length(app.variables$ref.objects))
                   {
                     ref <- app.variables$ref.objects[[i]]
                     if(length(ref)>0)
                     {
                       updateCheckboxInput(session, paste0("t_8_ref_",i), value = T)
                     }
                   }
                 }
               })

  observeEvent(input$t_8_deselect_all,
               {
                 if(length(app.variables$ref.objects)>0)
                 {
                   for(i in 1:length(app.variables$ref.objects))
                   {
                     ref <- app.variables$ref.objects[[i]]
                     if(length(ref)>0)
                     {
                       updateCheckboxInput(session, paste0("t_8_ref_",i), value = F)
                     }
                   }
                 }
               })

  observeEvent(input$t_8_dl_prepare,
               {

                 shinyjs::disable("t_8_dl_prepare")
                 files.names <- c()
                 nmb.files <- 0

                 tmp.dir <- tempdir()
                 dir.create(tmp.dir)
                 setwd(tmp.dir)
                 zip.output.file <- paste0("output.zip")
                 env.var$zip.output <- paste0(tmp.dir, "/", zip.output.file)

                 progress <- Progress$new()
                 progress$set("Preparing zip arcihve", value=0)

                 if(length(app.variables$ref.objects)>0)
                 {
                   for(i in 1:length(app.variables$ref.objects))
                   {
                     ref <- app.variables$ref.objects[[i]]
                     if(length(ref)>0)
                     {
                       if(input[[paste0("t_8_ref_",i)]])
                       {
                         nmb.files <- nmb.files + 1
                         if(length(app.variables$group.objects)>0 && ref$Name %in%names(app.variables$group.objects) &&
                            length(app.variables$group.objects[[ref$Name]])>0)
                         {
                           nmb.files <- nmb.files + length(app.variables$group.objects[[ref$Name]])
                         }
                       }
                     }
                   }
                 }

                 progress$inc(1/(nmb.files+2), detail=paste0(nmb.files, " files detected"))

                 if(nmb.files>0)
                 {
                   if(length(app.variables$ref.objects)>0)
                   {
                     tmp.dir <- tempdir()
                     for(i in 1:length(app.variables$ref.objects))
                     {
                       ref <- app.variables$ref.objects[[i]]
                       if(length(ref)>0)
                       {
                         if(input[[paste0("t_8_ref_",i)]])
                         {

                           fcs <- save.object.as.FCS(ref$FCSG2)

                           dir.create(tmp.dir)

                           tmp.name <- paste0(tmp.dir,"/",ref$Name, ".fcs")
                           write.FCS(fcs, tmp.name)
                           files.names <- c(unlist(files.names), tmp.name)
                           progress$inc(1/(nmb.files+2), detail=paste0(app.variables$fcs.files[[i]][["name"]], " added to archive"))
                           #==

                           if(length(app.variables$group.objects)>0 && ref$Name %in%names(app.variables$group.objects) &&
                              length(app.variables$group.objects[[ref$Name]])>0)
                           {
                             for(j in 1:length(app.variables$group.objects[[ref$Name]]))
                             {
                               obj <- app.variables$group.objects[[ref$Name]][[j]]
                               tmp.name <- paste0(tmp.dir,"/",obj$Name, ".fcs")
                               fcs <- save.object.as.FCS(obj$FCSG2)
                               write.FCS(fcs, tmp.name)
                               files.names <- c(unlist(files.names), tmp.name)
                               progress$inc(1/(nmb.files+2), detail=paste0(app.variables$fcs.files[[i]][["name"]], " added to archive"))
                             }
                           }
                         }
                       }
                     }
                   }
                   zip(zip.output.file, files.names)
                   file.remove(unlist(files.names))
                   setwd(env.var$tool.wd)
                   progress$inc(1/(nmb.files+2), detail="Zip archive generated")
                 }
                 progress$set("Zip ready", value=1)
                 delay(500, progress$close())
                 delay(500, shinyjs::enable("t_8_dl_prepare"))

               })

  output$t_8_dl_link <- renderUI(
    {
      dl.link <- NULL
      if(!is.null(env.var$zip.output) && file.exists(env.var$zip.output))
      {
        dl.link <- tags$a(href = str_replace(env.var$zip.output,"/media/data/html","http://10.71.1.22/"), "Download")
      }

      return(dl.link)
    })

  #LOCAL VERSION - DOWNLOAD FUNCTION
  #==============================================================================================================================================
  # output$t_8_dl <- downloadHandler(
  #     filename = function()
  #     {
  #         return("output.zip")
  #     },
  #     content= function(file)
  #     {
  #         shinyjs::disable("t_8_dl")
  #         files.names <- c()
  #         nmb.files <- 0
  #
  #         progress <- Progress$new()
  #         progress$set("Downloading files", value=0)
  #
  #         if(length(app.variables$ref.objects)>0)
  #         {
  #             for(i in 1:length(app.variables$ref.objects))
  #             {
  #                 ref <- app.variables$ref.objects[[i]]
  #                 if(length(ref)>0)
  #                 {
  #                     if(input[[paste0("t_8_ref_",i)]])
  #                     {
  #                         nmb.files <- nmb.files + 1
  #                         if(length(app.variables$group.objects)>0 && ref$Name %in%names(app.variables$group.objects) &&
  #                            length(app.variables$group.objects[[ref$Name]])>0)
  #                         {
  #                             nmb.files <- nmb.files + length(app.variables$group.objects[[ref$Name]])
  #                         }
  #                     }
  #                 }
  #             }
  #         }
  #
  #         progress$inc(1/(nmb.files+2), detail=paste0(nmb.files, " files detected"))
  #
  #         if(nmb.files>0)
  #         {
  #             if(length(app.variables$ref.objects)>0)
  #             {
  #                 tmp.dir <- tempdir()
  #                 for(i in 1:length(app.variables$ref.objects))
  #                 {
  #                     ref <- app.variables$ref.objects[[i]]
  #                     if(length(ref)>0)
  #                     {
  #                         if(input[[paste0("t_8_ref_",i)]])
  #                         {
  #
  #                             fcs <- save.object.as.FCS(ref$FCSG2)
  #
  #                             dir.create(tmp.dir)
  #
  #                             tmp.name <- paste0(tmp.dir,"/",ref$Name, ".fcs")
  #                             write.FCS(fcs, tmp.name)
  #                             files.names <- c(unlist(files.names), tmp.name)
  #                             progress$inc(1/(nmb.files+2), detail=paste0(app.variables$fcs.files[[i]][["name"]], " added to download list"))
  #                             #==
  #
  #                             if(length(app.variables$group.objects)>0 && ref$Name %in%names(app.variables$group.objects) &&
  #                                length(app.variables$group.objects[[ref$Name]])>0)
  #                             {
  #                                 for(j in 1:length(app.variables$group.objects[[ref$Name]]))
  #                                 {
  #                                     obj <- app.variables$group.objects[[ref$Name]][[j]]
  #                                     tmp.name <- paste0(tmp.dir,"/",obj$Name, ".fcs")
  #                                     fcs <- save.object.as.FCS(obj$FCSG2)
  #                                     write.FCS(fcs, tmp.name)
  #                                     files.names <- c(unlist(files.names), tmp.name)
  #                                     progress$inc(1/(nmb.files+2), detail=paste0(app.variables$fcs.files[[i]][["name"]], " added to download list"))
  #                                 }
  #                             }
  #                         }
  #                     }
  #                 }
  #             }
  #             zip(file, files.names)
  #             file.remove(unlist(files.names))
  #             file.remove(tmp.dir)
  #             progress$inc(1/(nmb.files+2), detail="Zip archive generated")
  #         }
  #         progress$set("Zip ready", value=1)
  #
  #         delay(500, progress$close())
  #         delay(500, shinyjs::enable("t_8_dl"))
  #     }
  # )
  #==============================================================================================================================================

  #===================================================================






  #INPUT UPDATES
  #===================================================================
  observe( #REFERENCE
    {
      update.ref.sel("t_1_files_rm_sel")
    })




  observe( #Generate Models - POP MODIFICATION
    {
      update.ref.sel("t_3_pop_ref_sel")
    })

  observe(
    {
      update.markers.sel("t_3_pop_move_markers_sel", "t_3_pop_ref_sel")
    })

  observe(
    {
      update.markers.sel("t_3_pop_m1","t_3_pop_ref_sel")
      update.markers.sel("t_3_pop_m2","t_3_pop_ref_sel")
    })




  observe( #Generate Models - TP
    {
      update.ref.sel("t_3_tp_ref_sel")
    })

  observe(
    {
      update.markers.sel("t_3_tp_M1", "t_3_tp_ref_sel")
      update.markers.sel("t_3_tp_M2", "t_3_tp_ref_sel")
    })




  observe( #Generate Models - MIX
    {
      update.ref.sel("t_3_mix_r1_sel")
      update.ref.sel("t_3_mix_r2_sel")
    })

  observe(
    {
      update.populations.sel("t_3_mix_r1_pop_sel","t_3_mix_r1_sel")
      update.populations.sel("t_3_mix_r2_pop_sel","t_3_mix_r2_sel")
    })



  observe( #Generate Groups
    {
      update.ref.sel("t_4_ref_sel")
    })

  observe(
    {
      update.markers.sel("t_4_shifted_markers", "t_4_ref_sel")
    })



  observe( #Visualization - HEATMAPS
    {
      update.ref.sel("t_6_hm_ref1_sel")
      update.ref.sel("t_6_hm_ref2_sel")
    })




  observe( #Visualization - JOYPLOT
    {
      update.ref.sel("t_6_jp_ref_sel")
    })



  observe( #Visualization - JOYPLOT
    {
      update.ref.sel("t_6_sc_ref_sel")
    })
  observe( #Visualization - JOYPLOT
    {
      update.markers.sel("t_6_sc_m1_sel", "t_6_sc_ref_sel")
      update.markers.sel("t_6_sc_m2_sel", "t_6_sc_ref_sel")
      update.populations.sel("t_6_sc_pop_sel", "t_6_sc_ref_sel")
    })
  #===================================================================

}

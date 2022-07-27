library(shiny)
library(shinydashboard)
library(shinyjs)
library(sunburstR)
library(heatmaply)
library(shinyHeatmaply)
library(shinybusy)


ui <- dashboardPage(
  dashboardHeader
  (
    title="FCS Generator 2.5"
  ),

  dashboardSidebar
  (
    sidebarMenu
    (
      id="tabs",
      checkboxInput("enable_comments", "Activer les messages d'aide", value=T),
      menuItem("Reference Files", tabName="t_1"),
      menuItem("Compensate & Transform", tabName="t_2"),
      menuItem("Generate Models", tabName="t_3",
               menuSubItem("Populations Modification", tabName = "t_3_pop"),
               menuSubItem("Pseudo-timeline", tabName = "t_3_tp"),
               menuSubItem("Mix Files", tabName = "t_3_mix")),
      menuItem("Generate Groups", tabName="t_4"),
      menuItem("Visualization Tools", tabName = "t_6",
               menuSubItem("Heatmaps", tabName = "t_6_hm"),
               menuSubItem("Joyplot", tabName = "t_6_jp"),
               menuSubItem("Scatter plot", tabName = "t_6_sc")),
      menuItem("Decompensate & Detransform", tabName="t_7"),
      menuItem("Download", tabName = "t_8")
    )
  ),

  dashboardBody(
    useShinyjs(),
    add_busy_spinner(spin = "breeding-rhombus", width="200px", height="200px"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    fluidRow(column(8,
                    tabItems(
                      tabItem(tabName="t_1",
                              column(12,
                                     shinydashboard::box(width=12,title="ADD FILES",solidHeader=T,status="info",
                                                         shinydashboard::tabBox(width=12,id="t_1_tb",
                                                                                tabPanel(title="Generate New File", value="A",
                                                                                         style="padding-right:0",
                                                                                         div(class="help_comment",HTML("<p>
                                        Generation de fichiers <b>[Number of Files]</b> fichiers independants. Chaque fichier comporte
                                        <b>[Number of Events]</b> events, <b>[Number of Markers]</b> markers et
                                        <b>[Number of Populations]</b> populations.
                                        </p>
                                        <p>
                                        La frequence de chaque population peut etre reglee en modifiant <b>Min Frequency</b> (frequence minimale
                                        voulue pour la population) et <b>Max Frequency</b> (frequence maximale autorisee).
                                        </p>
                                    ")),
                                                                                         fluidRow(style="margin-right:0",
                                                                                                  column(width=2,
                                                                                                         numericInput("t_1_nmb_files", "Number of Files", value=1),
                                                                                                         numericInput("t_1_nmb_events", "Number of Events", value=1000),
                                                                                                         numericInput("t_1_nmb_markers", "Number of Markers", value=2)
                                                                                                  ),
                                                                                                  column(width=3,
                                                                                                         numericInput("t_1_nmb_populations", "Number of Populations", value=1),
                                                                                                         textInput("t_1_generated_name", "Base Filename", value="filename"),
                                                                                                         actionButton("t_1_create", "Generate", width="100%")
                                                                                                  ),
                                                                                                  column(width=7,style="overflow:auto;padding:0",
                                                                                                         uiOutput("t_1_pop_list")
                                                                                                  )
                                                                                         )
                                                                                ),
                                                                                tabPanel(title="Import Files",value="B",
                                                                                         div(class="help_comment",HTML("
                                        <p>Utilisez le bouton <b>Add Files</b> pour selectionner des fichiers FCS sur votre machine.
                                        </p>
                                        <p>Pour chaque fichier importe, selectionner la variable contenant l'annotation des populations dans
                                        <b>Annotation Column</b> et la liste des marqueurs dans <b>Markers List</b>.
                                        </p>
                                        <p>Confirmez l'importation des fichiers avec <b>Validate</b>
                                        </p>
                                    ")),
                                                                                         fluidRow(column(width=12,style="max-height:35vh;overflow:auto;padding:0",
                                                                                                         uiOutput("t_1_fcs_list")
                                                                                         )),
                                                                                         fluidRow(style="margin-top:1.5vh",
                                                                                                  column
                                                                                                  (
                                                                                                    width=6,
                                                                                                    #  actionButton("t_1_select", "Add Files", width="100%")
                                                                                                    fileInput(
                                                                                                      "t_1_select",
                                                                                                      "Load .FCS/.TXT/.CSV File(s) from your desktop",
                                                                                                      multiple = TRUE,
                                                                                                      accept = c(
                                                                                                        "text/csv",
                                                                                                        "text/comma-separated-values,text/plain",
                                                                                                        ".csv",
                                                                                                        ".fcs"
                                                                                                      )
                                                                                                    )
                                                                                                  ),
                                                                                                  column(width=6,
                                                                                                         actionButton("t_1_fcs_validate", "Validate", width = "100%")
                                                                                                  )
                                                                                         )
                                                                                )
                                                         ),
                                                         column(width=12,fluidRow(uiOutput("t_1_files_main")))
                                     )
                              )
                      ),
                      tabItem(tabName="t_2",
                              column(width=3,
                                     shinydashboard::box(width=12,title="Compensation",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                                                         actionButton("t_2_compensate", "Compensate Selection", width="100%")
                                     ),
                                     shinydashboard::box(width=12,title="Transformation",solidHeader=T,status="info",style="max-height:70vh;overflow:auto",
                                                         selectInput("t_2_transform_sel", "Transformation Type", choices=c("Logicle"=1, "Arcshinh"=2)),
                                                         uiOutput("t_2_transform_param"),
                                                         actionButton("t_2_transform", "Transform Selection", width="100%")
                                     )
                              ),
                              column(width=9,
                                     shinydashboard::box(width=12,title="Reference Files",solidHeader=T,status="info",style="overflow:auto",
                                                         fluidRow(
                                                           column(width=2,
                                                                  checkboxInput("t_2_file_select_all", "Select All", value = F)
                                                           ),
                                                           column(width=4,style="padding-top:1%",
                                                                  "Reference File"
                                                           ),
                                                           column(width=3,style="padding-top:1%",
                                                                  "Compensated"
                                                           ),
                                                           column(width=3,style="padding-top:1%",
                                                                  "Transformed"
                                                           )
                                                         ),
                                                         uiOutput("t_2_files")
                                     )
                              )
                      ),
                      tabItem(tabName="t_3_pop",
                              column(width=12,
                                     shinydashboard::box(width=12,title="Generate a model by modifying each population in a reference",solidHeader=T,status="info",
                                                         div(class="help_comment",HTML("
                                <p>Chaque onglet permet d'apporter un type de modifications aux populations d'une copie d'un fichier reference.
                                Utilisez <b>Generate Model</b> pour valider les modifications et enregistrer le nouveau model sous le nom
                                <b>Model Name</b>
                                </p>
                            ")),
                                                         fluidRow(
                                                           column(width=4,
                                                                  selectInput("t_3_pop_ref_sel", "Reference File", choices = NULL, selected = NULL)
                                                           ),
                                                           column(width=4,
                                                                  textInput("t_3_pop_model_name", "Model Name", value="GROUP")
                                                           ),
                                                           column(width=4,
                                                                  actionButton("t_3_pop_generate", "Generate Model", style="margin-top:2.2vh")
                                                           )
                                                         ),
                                                         fluidRow(
                                                           shinydashboard::tabBox(width=12,id="t_3_pop_tab",
                                                                                  tabPanel(title=tags$b("Move Populations"), value="A",
                                                                                           div(class="help_comment",HTML("
                                                <p>Deplacez la MFI (moyenne et ecart-type) de la <b>population</b> selectionnee pour tous les <b>markers</b>
                                                choisis, via les curseurs sous chaque histogramme dans <i><b>MFI of the translated population</b></i>
                                                </p>
                                            ")
                                                                                           ),
                                                                                           fluidRow(
                                                                                             column(width=3,
                                                                                                    selectInput("t_3_pop_move_pop_sel", "Population", choices = NULL, selected = NULL)
                                                                                             ),
                                                                                             column(width=5,
                                                                                                    selectInput("t_3_pop_move_markers_sel", "Markers", choices = NULL, selected = NULL, multiple = T)
                                                                                             )
                                                                                           )
                                                                                  ),
                                                                                  tabPanel(title=tags$b("Add/Remove Populations"), value="B",
                                                                                           div(class="help_comment",HTML("
                                            <p>Supprimez une <b>population</b> ou en ajouter une avec <b>[Number of Events]</b> events en utilisant les
                                            MFI d'une <b>population reference</b>
                                            </p>
                                        ")),
                                                                                           fluidRow(
                                                                                             shinydashboard::box(width=5,title="Delete Population",
                                                                                                                 fluidRow
                                                                                                                 (
                                                                                                                   width=12,
                                                                                                                   column
                                                                                                                   (
                                                                                                                     width=7,
                                                                                                                     selectInput("t_3_pop_manage_rm_sel", "Populations", choices = NULL)
                                                                                                                   ),
                                                                                                                   column
                                                                                                                   (
                                                                                                                     width=5,style="padding-top:2vh",
                                                                                                                     actionButton("t_3_pop_manage_rm", "Remove", width="100%")
                                                                                                                   )
                                                                                                                 )
                                                                                             ),
                                                                                             shinydashboard::box
                                                                                             (
                                                                                               width=7,
                                                                                               title="Add Population",
                                                                                               fluidRow
                                                                                               (
                                                                                                 width=6,
                                                                                                 column
                                                                                                 (
                                                                                                   width=4,
                                                                                                   selectInput("t_3_pop_manage_add_sel", "Reference Population", choices = NULL)
                                                                                                 ),
                                                                                                 column
                                                                                                 (
                                                                                                   width=4,
                                                                                                   numericInput("t_3_pop_manage_add_events", "Number of Events", min = 0, value = 100)
                                                                                                 ),
                                                                                                 column
                                                                                                 (
                                                                                                   width=4,style="padding-top:2vh",
                                                                                                   actionButton("t_3_pop_manage_add", "Add", width="100%")
                                                                                                 )
                                                                                               )
                                                                                             )
                                                                                           )
                                                                                  ),
                                                                                  tabPanel
                                                                                  (
                                                                                    title=tags$b("Change Population Size"), value="C",
                                                                                    div
                                                                                    (
                                                                                      class="help_comment",
                                                                                      HTML
                                                                                      ("
                                                <p>
                                                    Utilisez le curseur ou le champs de texte pour modifier la <b>taille relative</b> d'une <b>population</b>.
                                                    Une valeur de 50% indique que la population contient moitie moins d'events. Le nombre d'events total du fichier
                                                    est modifiee.
                                                </p>
                                                ")
                                                                                    ),
                                                                                    fluidRow
                                                                                    (
                                                                                      column
                                                                                      (
                                                                                        width=3,
                                                                                        selectInput("t_3_pop_size_pop_sel", "Populations", choices = NULL),
                                                                                        column
                                                                                        (
                                                                                          width=12,
                                                                                          h4("Number of events: "),
                                                                                          uiOutput("t_3_pop_size_nmb_events")
                                                                                        )
                                                                                      ),
                                                                                      column
                                                                                      (
                                                                                        width=7,
                                                                                        sliderInput("t_3_pop_size_slider", "Relative size (%)", min = 0, max = 200, value = 100, step = 0.0001)
                                                                                      ),
                                                                                      column
                                                                                      (
                                                                                        width=2,style="padding-top:2%",
                                                                                        numericInput("t_3_pop_size_text", "", value = 100, min = 0)
                                                                                      )
                                                                                    )
                                                                                  )
                                                           )
                                                         )
                                     )
                              ),
                              column
                              (
                                width=12,
                                shinydashboard::box
                                (
                                  width=6,solidHeader=T, status="info",style="height:50vh;overflow:auto",
                                  title="MFI of the translated population",
                                  uiOutput("t_3_pop_ui")
                                ),
                                shinydashboard::box
                                (
                                  width=6,solidHeader=T, status="info",style="height:50vh;overflow:auto",
                                  title="Visualization Tool",
                                  fluidRow
                                  (
                                    column
                                    (
                                      width=3,
                                      selectInput("t_3_pop_m1", "1st Marker", choices = NULL, selected = NULL)
                                    ),
                                    column
                                    (
                                      width=3,
                                      selectInput("t_3_pop_m2", "2nd Marker", choices = NULL, selected = NULL)
                                    ),
                                    column
                                    (
                                      width=6,
                                      selectInput("t_3_pop_hp", "Highlighted populations", choices = NULL, selected = NULL, multiple = T)
                                    )
                                  ),
                                  plotOutput("t_3_pop_plot")

                                )
                              )
                      ),
                      #===============================================================================================





                      #Generate Models (TIMEPOINTS)
                      #===============================================================================================
                      tabItem
                      (
                        tabName="t_3_tp",
                        column
                        (
                          width=12,
                          div
                          (
                            class="help_comment", style="margin-bottom:1vh",
                            HTML
                            ("
                                <p>
                                    Permet de simuler la diminution du nombre d'events de populations au cours du temps. Chaque Point Temporel (TP) correspond a
                                    la reduction simultanee du nombre d'events des population ayant un <b>Reduction %</b> superieur a 0. Un nombre d'events
                                    equivalent est attribue a toutes les populations libres, i.e les non <b>Locked Populations</b>
                                </p>
                                <p>
                                    L'ensemble des TP permet de simuler un pseudo-temps.
                                    Le nombre total d'events est conserve.
                                    Pensez a <b>Mettre a jour (Update)</b> apres chaque modification.
                                </p>
                                ")

                          )
                        ),
                        column
                        (
                          width=3,
                          shinydashboard::box
                          (
                            width=12,title="Generate Models with Time Points",solidHeader=T,status="info",
                            fluidRow
                            (
                              column
                              (
                                width=12,
                                selectInput("t_3_tp_ref_sel", "Reference File", choices = NULL, selected = NULL),
                                actionButton("t_3_tp_add", "Add a TP", width="100%"),
                                actionButton("t_3_tp_update", "Update", width="100%"),
                                actionButton("t_3_tp_generate", "Generate the Models", width = "100%")
                              ),
                              column
                              (
                                width=12, style="margin-top:5vh",
                                selectInput("t_3_tp_remove_sel", "TP List", choices = NULL, selected = NULL, multiple = T),
                                actionButton("t_3_tp_remove", "Remove", width="100%")
                              )
                            )
                          )
                        ),
                        column
                        (
                          width=9,
                          shinydashboard::box
                          (
                            width=12,title="Visualization Options",solidHeader=T,status="info",
                            fluidRow
                            (
                              column
                              (
                                width=6,
                                selectInput("t_3_tp_M1", "1st Marker", choices = NULL, selected = NULL)
                              ),
                              column
                              (
                                width=6,
                                selectInput("t_3_tp_M2", "2nd Marker", choices = NULL, selected = NULL)
                              )
                            )
                          ),
                          uiOutput("t_3_tp_body")
                        )
                      ),
                      #===============================================================================================





                      #Generate Models (MIX FILES)
                      #===============================================================================================
                      tabItem
                      (
                        tabName="t_3_mix",
                        column
                        (
                          width=12,
                          shinydashboard::box
                          (
                            width=12,solidHeader=T, status="info",
                            title="Generate a model by mixing two models / references",
                            div
                            (
                              class="help_comment", style="margin-bottom:1vh",
                              HTML
                              ("
                                    <p>
                                        Generer un modele par combinaison de 2 modeles / references. Les proportions peuvent etre modifiees
                                        avec les <b>Extraction %</b>. Utilisez <b>Preview The File</b> pour generer un apercu du fichier dans le
                                        <i><b>Scatter Plot</b></i>, modifiable avec les <i><b>Visualization Options</b></i>.
                                    </p>
                                    ")

                            ),
                            column
                            (
                              width=5,
                              column
                              (
                                width=12,
                                h4("Select the Models")
                              ),
                              fluidRow
                              (
                                column
                                (
                                  width= 8,
                                  selectInput("t_3_mix_r1_sel", "1st Reference File", choices = NULL, selected = NULL)
                                ),
                                column
                                (
                                  width = 4,
                                  numericInput("t_3_mix_r1_p", "Extraction %", value = 100, min = 0, max = 100)
                                )
                              ),
                              fluidRow
                              (
                                column
                                (
                                  width= 8,
                                  selectInput("t_3_mix_r2_sel", "Extraction %", choices = NULL, selected = NULL)
                                ),
                                column
                                (
                                  width = 4,
                                  numericInput("t_3_mix_r2_p", "%", value = 100, min = 0, max = 100)
                                )
                              ),
                              fluidRow
                              (
                                style="margin-top:2vh",
                                column
                                (
                                  width= 6,
                                  actionButton("t_3_mix_generate", "Preview The File", width = "100%")
                                ),
                                column
                                (
                                  width = 6,
                                  actionButton("t_3_mix_validate", "Generate the Model", width = "100%")
                                )
                              )
                            ),
                            column
                            (
                              width=7, style="border-left:1px solid black",
                              column
                              (
                                width=12,
                                h4("Visualization Options")
                              ),
                              column
                              (
                                width=12,
                                selectInput("t_3_mix_m1", "1st Marker", choices = NULL, selected = NULL)
                              ),
                              column
                              (
                                width=12,
                                selectInput("t_3_mix_m2", "2nd Marker", choices = NULL, selected = NULL)
                              ),
                              column
                              (
                                width=12,
                                selectInput("t_3_mix_hp", "Highlighted populations", choices = NULL, selected = NULL, multiple = T)
                              )
                            )
                          ),
                          shinydashboard::box
                          (
                            width=12,solidHeader=T, status="info",
                            title="Scatter Plot",
                            plotOutput("t_3_mix_plot")
                          )
                        )
                      ),
                      #===============================================================================================







                      #Generate Groups
                      #===============================================================================================
                      tabItem
                      (
                        tabName="t_4",
                        shinydashboard::box
                        (
                          width=12,title="Generate a group from a model",solidHeader=T,status="info",
                          div
                          (
                            class="help_comment", style="margin-bottom:1vh",
                            HTML
                            ("
                                <p>
                                    Generez un groupe ou une cohorte a partir d'un model choisi dans <b>Reference File</b>. Les <b>Shifted Markers</b>
                                    correspondent aux marqueurs pour lesquels une modification de la MFI de chaque population autour de sa position d'origine
                                    autorisee. <b>Max Mean shift</b> permet de donner un deplacement maximal des moyennes autour des moyennes actuelles des
                                    populations. <b>Max SD shift</b> limite le deplacement de l'ecart-type. Les valeurs de la moyenne et l'ecart-type sont
                                    tirees aleatoirement sur une loi uniforme pour chaque population.
                                </p>
                                ")

                          ),
                          column
                          (
                            width=12,
                            fluidRow
                            (
                              width=12,
                              column
                              (
                                width=3,
                                selectInput("t_4_ref_sel", "Reference File", choices = NULL, selected = NULL)
                              ),
                              column
                              (
                                width=4,
                                selectInput("t_4_shifted_markers", "Shifted Markers", choices = NULL, selected = NULL, multiple = T)
                              ),
                              column
                              (
                                width=2,
                                numericInput("t_4_nmb_files", "Number of Files", value=1)
                              ),
                              column
                              (
                                width=2,
                                actionButton("t_4_generate", "Generate", width="100%", style="margin-top:2.5vh")
                              )
                            ),
                            fluidRow
                            (
                              width=12,

                              column
                              (
                                width=2,
                                numericInput("t_4_mean_shift", "Max Mean shift", value = 0.8)
                              ),
                              column
                              (
                                width=2,
                                numericInput("t_4_sd_shift", "Max SD shift", value = 0.2)
                              )
                            )
                          )
                        ),
                        shinydashboard::box
                        (
                          width=12,title="Model - Heatmap",solidHeader=T,status="info", collapsible=T,
                          plotlyOutput("t_4_hm_ref", width = "100%")
                        ),
                        uiOutput("t_4_hm_list")
                      ),
                      #===============================================================================================





                      #VISUALIZATIONS (HEATMAPS)
                      #===============================================================================================
                      tabItem
                      (
                        tabName="t_6_hm",
                        shinydashboard::box
                        (
                          title="File Options",solidHeader=T,status="info",width=12,
                          fluidRow
                          (
                            column
                            (
                              width=6,
                              selectInput("t_6_hm_ref1_sel", "1st Reference File", choices = NULL, selected = NULL)
                            ),
                            column
                            (
                              width=6,
                              selectInput("t_6_hm_ref2_sel", "2nd Reference File", choices = NULL, selected = NULL)
                            )
                          )
                        ),
                        shinydashboard::box
                        (
                          title="Sunburst Plots",solidHeader=T,status="info",width=12,collapsible=T, collapsed=T,
                          column
                          (
                            width=6,
                            sunburstOutput("t_6_hm_sb_1")
                          ),
                          column
                          (
                            width=6,
                            sunburstOutput("t_6_hm_sb_2")
                          )
                        ),
                        shinydashboard::box
                        (
                          title="Heatmaps",solidHeader=T,status="info",width=12,collapsible=T,
                          column
                          (
                            width=6,
                            plotlyOutput("t_6_hm_1", width = "100%")
                          ),
                          column
                          (
                            width=6,
                            plotlyOutput("t_6_hm_2", width = "100%")
                          )
                        )
                      ),
                      #===============================================================================================




                      #VISUALIZATIONS (JOYPLOTS)
                      #===============================================================================================
                      tabItem
                      (
                        tabName="t_6_jp",
                        shinydashboard::box
                        (
                          title="File Options",solidHeader=T,status="info",width=12,
                          column
                          (
                            width=6,
                            selectInput("t_6_jp_ref_sel", "Reference Files", choices = NULL, selected = NULL, multiple = T)
                          ),
                          column
                          (
                            width=6,
                            selectInput("t_6_jp_marker_sel", "Marker", choices = NULL, selected = NULL)
                          )
                        ),
                        shinydashboard::box
                        (
                          title="Joyplot",solidHeader=T,status="info",width=12,collapsible=T,
                          plotOutput("t_6_jp", width = "100%")
                        )
                      ),
                      #===============================================================================================




                      #VISUALIZATIONS (SCATTER PLOT)
                      #===============================================================================================
                      tabItem
                      (
                        tabName="t_6_sc",
                        shinydashboard::box
                        (
                          title="File Options",solidHeader=T,status="info",width=12,
                          fluidRow
                          (
                            column
                            (
                              width=6,
                              selectInput("t_6_sc_ref_sel", "Reference Files", choices = NULL, selected = NULL)
                            ),
                            column
                            (
                              width=6,
                              selectInput("t_6_sc_pop_sel", "Highlighted Populations", choices = NULL, selected = NULL)
                            )
                          ),
                          fluidRow
                          (
                            column
                            (
                              width=6,
                              selectInput("t_6_sc_m1_sel", "1st Marker", choices = NULL, selected = NULL)
                            ),
                            column
                            (
                              width=6,
                              selectInput("t_6_sc_m2_sel", "2nd Marker", choices = NULL, selected = NULL)
                            )
                          )
                        ),
                        shinydashboard::box
                        (
                          title="Scatter plot",solidHeader=T,status="info",width=12,collapsible=T,
                          plotOutput("t_6_sc", width = "100%")
                        )
                      ),
                      #===============================================================================================




                      #Decompensate and Detransform
                      #===============================================================================================
                      tabItem
                      (
                        tabName="t_7",
                        column
                        (
                          width=4,
                          shinydashboard::box
                          (
                            width=12,title="Compensation",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            actionButton("t_7_decompensate", "Decompensate Selection", width="100%")
                          ),
                          shinydashboard::box
                          (
                            width=12,title="Transformation",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            selectInput("t_7_detransform_sel", "Transformation Type", choices=c("Logicle"=1, "Arcshinh"=2)),
                            uiOutput("t_7_detransform_param"),
                            actionButton("t_7_detransform", "Detransform Selection", width="100%")
                          )
                        ),
                        column
                        (
                          width=8,
                          shinydashboard::box
                          (
                            width=12,title="Reference Files",solidHeader=T,status="info",style="max-height:30vh;overflow:auto",
                            fluidRow
                            (
                              column
                              (
                                width=2,
                                checkboxInput("t_7_file_select_all", "Select All", value = F)
                              ),
                              column
                              (
                                width=4,style="padding-top:1%",
                                "Reference File"
                              ),
                              column
                              (
                                width=3,style="padding-top:1%",
                                "Compensated"
                              ),
                              column
                              (
                                width=3,style="padding-top:1%",
                                "Transformed"
                              )
                            ),
                            uiOutput("t_7_files")
                          )
                        )
                      ),
                      #===============================================================================================





                      #Download
                      #===============================================================================================
                      tabItem
                      (
                        tabName="t_8",
                        column
                        (
                          width=8,
                          shinydashboard::box
                          (
                            width=12,title="Select Files",solidHeader=T,status="info",style="overflow:auto",
                            fluidRow
                            (
                              column
                              (
                                width=1,
                                h4(tags$b("Select"))
                              ),
                              column
                              (
                                width=9,
                                h4(tags$b("File"))
                              ),
                              column
                              (
                                width=2,
                                h4(tags$b("Type"))
                              )
                            ),
                            uiOutput("t_8_files")
                          )
                        ),
                        column
                        (
                          width=4,
                          shinydashboard::box
                          (
                            width=12,title="Download Options",solidHeader=T,status="info",style="overflow:auto",
                            fluidRow
                            (
                              column
                              (
                                width=12,
                                actionButton("t_8_select_all", "Select All", width = "100%")
                              )
                            ),
                            fluidRow
                            (
                              column
                              (
                                width=12,
                                actionButton("t_8_deselect_all", "Deselect All", width = "100%")
                              )
                            ),
                            fluidRow
                            (
                              column
                              (
                                width=12,
                                actionButton("t_8_dl_prepare", "Prepare zip", width = "100%")
                              )
                            ),
                            uiOutput("t_8_dl_link")
                          )
                        )
                      )
                      #===============================================================================================
                    )
    ),
    shinyjs::hidden(div
                    (
                      id="global_files_div",
                      shinydashboard::box
                      (
                        width=4,title="Model Files",solidHeader=T,status="info", style="max-height:80vh;overflow:auto",
                        div
                        (
                          class="help_comment",
                          HTML
                          ("<p>
                            Liste des modeles pouvant etre utilises pour generer des groupes / cohorts
                          </p>
                         ")

                        ),
                        fluidRow
                        (
                          column
                          (
                            width=1
                          ),
                          column
                          (
                            width=1,
                            p(tags$b("Size"))
                          ),
                          column
                          (
                            width = 3,
                            p(tags$b("Name"))
                          ),
                          column
                          (
                            width = 2,
                            p(tags$b("Populations"))
                          ),
                          column
                          (
                            width = 2,
                            p(tags$b("Markers"))
                          ),
                          column
                          (
                            width = 3,
                            p(tags$b("Events"))
                          )
                        ),
                        uiOutput("global_files_list"),
                        actionButton("global_files_sel_all", "Select All", width="40%", style="margin-left:10%"),
                        actionButton("global_files_desel_all", "Deselect All", width="40%"),
                        actionButton("global_files__rm", "Remove Selection", width="80%", style='margin-left:10%')
                      )
                    ))

    )

  )

)

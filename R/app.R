#' MakeNGSfilter
#'
#' Produces NGSfilter files
#' 
#' @details
#' This Shiny app allows users to produce NGSfilter files from three input files:
#' 
#' - a 'comments' file: a tab-separated table that lists all samples and controls and their characteristics;
#' 
#' - a 'PCR plates design' file: a tab-separated file that represents the PCR plates design;
#' 
#' - a 'tags plates design' file: a .xlsx files that represents the distribution of sample tags in the PCR plates context.
#' 
#' All input files are checked to avoid bugs when used in downstream programs and functions.
#' 
#' The output files can be used as input of the \code{ngsfilter} program of the OBITools (Boyer et al. 2016) and the \code{obifiles_to_metabarlist} function of the metabaR R package (Zinger et al. 2021).
#' 
#' 
#' @author Anne-Sophie Benoiston
#' 
#' @references Boyer, F., Mercier, C., Bonin, A., Bras, Y. L., Taberlet, P., & Coissac, E. (2016). obitools : A unix-inspired software package for DNA metabarcoding. Molecular Ecology Resources, 16(1), 176‑182. https://doi.org/10.1111/1755-0998.12428
#' @references Zinger, L., Lionnet, C., Benoiston, A.-S., Donald, J., Mercier, C., & Boyer, F. (2021). metabaR : An r package for the evaluation and improvement of DNA metabarcoding data quality. Methods in Ecology and Evolution, 12(4), 586‑592. https://doi.org/10.1111/2041-210X.13552
#'
#' @examples
#' \donttest{
#' MakeNGSfilter() # opens the shiny app in RStudio viewer or web navigator
#' }
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom stats var
#' @importFrom utils read.csv read.table write.table
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_length
#' 
#' @export MakeNGSfilter

MakeNGSfilter <- function(){
  # ui ----
  ui <- tagList(
    dashboardPage(
      dashboardHeader(title = "MakeNGSfilter"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("How to", tabName = "how_to", icon = icon("circle-question")),
          menuItem("Check files", tabName = "check_files", icon = icon("check")),
          menuItem("Make ngsfilter", tabName = "make_ngsfilter", icon = icon("gears")),
          menuItem("Contact", tabName = "contact", icon = icon("envelope"))
        )
      ),
      dashboardBody(
        tabItems(
          ## tab: how to ----
          tabItem(tabName = "how_to",
                  h2("What is the purpose of this Shiny app?"),
                  p("This app was written in order to make it easier to create ngsfilter files used by the ", a(href = 'https://pythonhosted.org/OBITools/welcome.html', 'OBItools', .noWS = "outside"), " pipeline (Boyer et al. 2016) and the ", a(href = 'https://github.com/metabaRfactory/metabaR/', 'metabaR', .noWS = "outside"), " R package (Zinger et al. 2021).", .noWS = c("after-begin", "before-end")),
                  h2("How to use this app?"),
                  h3("Mandatory files"),
                  p("Three files are necessary to produce a ngsfilter file:"),
                  tags$div(tags$ul(
                    tags$li("a 'comments' file: a tab-separated table that lists all samples and controls and their caracteristics;"),
                    tags$li("a 'PCR plates design' file: a tab-separated file that represents the PCR plates design;"),
                    tags$li("a 'tags plates design' file: a .xlsx files that represents the distribution of sample tags in the PCR plates context.")
                  )
                  ),
                  h3("Files structure"),
                  p("Each file must meet specific requirements to create correct ngsfilter files."),
                  h4("Comments"),
                  p("Celui-ci possède toutes les informations des échantillons et doit faire appel aux mêmes règles que pour le nommage des échantillons (pas d’accents, espace ou caractère spéciaux). Plusieurs colonnes sont utiles :"),
                  tags$div(tags$ul(
                    tags$li("'id' : colonne avec le nom des échantillons ;"),
                    tags$li("'type' : 'sample' pour les échantillons et 'control' pour les contrôles ;"),
                    tags$li("'control_type' : elle peut prendre 4 valeurs différentes pour les contrôles ('extraction', 'pcr', 'sequencing', 'positive' ; pour plus d'informations ", a(href = 'https://metabarfactory.github.io/metabaR/articles/metabaRF-vignette.html', 'visitez cette page', .noWS = "outside"),"). Cette colonne ne doit pas être complétée pour les échantillons.", .noWS = c("after-begin", "before-end"))
                  ),
                  h4("PCR plates design"),
                  h4("Tags plates design"),
                  h3("Example files"),
                  p("Add examples files to download here")
                  
                  )
          ),
          ## tab: check_files ----
          tabItem(tabName = "check_files",
                  fluidRow(
                    box(title = "Comments", width = 3, status = "primary",
                        fileInput(inputId = "comments", label = "", accept = c(".txt", ".tsv", ".tab"), buttonLabel = icon("arrow-up-from-bracket")),
                        actionButton("submit1", "Check"),
                        br(),
                        br(),
                        htmlOutput("text1")
                    ),
                    box(title = "Plate design", width = 3, status = "primary",
                        fileInput(inputId = "platedesign", label = "", accept = c(".txt", ".tsv", ".tab"), buttonLabel = icon("arrow-up-from-bracket")),
                        textInput(inputId = "unused_well", label = p("Word designating unused wells:"), value = ""),
                        actionButton("submit2", "Check"),
                        br(),
                        br(),
                        htmlOutput("text2")
                    ),
                    box(title = "Tags design", width = 3, status = "primary",
                        fileInput(inputId = "tags", label = "", accept = c(".xlsx"), buttonLabel = icon("arrow-up-from-bracket")),
                        actionButton("submit3", "Check"),
                        br(),
                        br(),
                        htmlOutput("text3")
                    ),
                    box(title = "Consistency", width = 3, status = "warning",
                        actionButton("submit4", "Check"),
                        br(),
                        br(),
                        htmlOutput("text4")
                    )
                  )
          ),
          ## tab: make ngsfilter ----
          tabItem(tabName = "make_ngsfilter",
                  fluidRow(
                    column(width = 3,
                           textInput(inputId = "experiment_name", label = p("Experiment name:"), value = "my_experiment"),
                           #selectInput("primers", "Primers:", choices = ""),
                           #uiOutput("new"),
                           textInput(inputId = "fwd_primer", label = "Forward primer sequence:", value = ""),
                           textInput(inputId = "rev_primer", label = "Reverse primer sequence:", value = ""),
                           textInput(inputId = "to_add", label = "(Optional) short character string to add at the end of samples/controls names:", value = ""),
                           textInput(inputId = "output_file_name", label = "Output file name:", value = "ngsfilter.tab"),
                           actionButton("make_ngsfilter", "Make ngsfilter file"),
                           br(),
                           br(),
                           htmlOutput("text5"),
                           htmlOutput("text6"),
                           br(),
                           br(),
                           downloadButton("downloadData", "Download")
                    ),
                    column(width = 9,
                           tableOutput("table")
                    )
                  )
          ),
          tabItem(tabName = "contact",
                  h5("This app was written by Anne-Sophie Benoiston with inputs from Lucie Moreau. For any question or suggestion, please send an email to anne-sophie.benoiston@ird.fr")
          )
        ),
      ),
      
    ),
    tags$footer(HTML("<i class='fa-regular fa-copyright'></i> Anne-Sophie Benoiston"), style = "
              position:fixed;
              bottom: 0;
              width: 100%;
              height: 50px;   /* Height of the footer */
              color: white;
              padding: 20px;
              z-index: 1000;")
  )
  
  
  #primers <- vroom::vroom("primers.tsv")
  comments_ok <- reactiveVal(FALSE)
  platedesign_ok <- reactiveVal(FALSE)
  tags_ok <- reactiveVal(FALSE)
  
  #  server ----
  server <- function(input, output, session) {
    
    # check comments files ----
    observeEvent(input$submit1, {
      file1 <- input$comments
      
      output$text1 <- renderText({
        
        validate(
          need(file1, 'Please upload a file!')
        )
        
        ext <- tools::file_ext(file1$datapath)
        validate(
          need(ext %in% c("txt", "tsv", "tab"),
               "Please upload a txt, tsv or tab file")
        )
        
        comments = read.table(file = file1$datapath, sep = "\t", header = T, stringsAsFactors = F, na.strings = "", check.names = FALSE)
        
        names_ok = c("id", "type", "control_type")
        e1 = all(grepl("^[A-Za-z0-9\\_\\.-]+$", colnames(comments)))
        e2 = sum(colnames(comments)[1:3] != names_ok) == 0
        e3 = all(grepl("^[A-Za-z0-9\\_\\.\\+-]+$", comments$id))
        e4 = sum(duplicated(comments$id)) == 0
        e5 = all(comments$type %in% c("control", "sample"))
        e6 = all(is.na(comments$control_type[comments$type == "sample"]))
        e7 = all(comments$control_type[comments$type == "control"] %in% c("extraction", "pcr", "positive", "sequencing"))
        # e8 = sum(grepl("^Tneg", comments$id[!is.na(comments$control_type)][comments$control_type[!is.na(comments$control_type)] == "pcr"])) == length(comments$id[!is.na(comments$control_type)][comments$control_type[!is.na(comments$control_type)] == "pcr"])
        
        # 1. check column names - authorized characters
        validate(
          need(expr = e1,
               message = paste0("At least one of your column names does not contain authorized characters ([A-Z] [a-z] [0-9] _ - .):\n",
                                paste0(colnames(comments)[!grepl("^[A-Za-z0-9\\_\\.-]+$", colnames(comments))], collapse = "\n"))
          )
        )
        
        # 2. check column names - three first
        validate(
          need(expr = e2, paste0("The first three columns should be named 'id', 'type' and 'control_type'.\nYour first three columns are named ", paste0(colnames(comments)[1:3], collapse = ", ")))
        )
        
        # 3. check sample names
        validate(
          need(expr = e3,
               message = paste0("At least one of your sample/control names does not contain authorized characters ([A-Z] [a-z] [0-9] _ - +):\n",
                                paste0(comments$id[!grepl("^[A-Za-z0-9\\_\\.\\+-]+$", comments$id)],
                                       collapse = "\n")))
        )
        
        # 4. duplicated sample names
        validate(
          need(expr = e4,
               message = paste0("At least of of your sample/control names are duplicated: \n",
                                paste0(comments$id[duplicated(comments$id)],
                                       collapse = "\n")))
        )
        
        # 5. check content of "type"
        validate(
          need(expr = e5,
               message = paste0("At least one of your samples/controls have wrong types:\n",
                                paste0(comments$id[!grepl("^sample$|^control$", comments$type)],
                                       collapse = "\n"))
               
          )
        )
        
        # 6. check content of "control_type" for samples
        validate(
          need(expr = e6,
               message = paste0("At least one of your samples have a wrong control type:\n",
                                paste0(comments$id[comments$type == "sample"][!is.na(comments$control_type[comments$type == "sample"])],
                                       collapse = "\n"), "\nSamples control type should be empty."))
        )
        
        # 7. check content of "control_type" for controls
        validate(
          need(e7,
               message = paste0("At least one of your controls have a wrong control type:\n",
                                paste0(comments$id[comments$type == "control"][!grepl("^pcr$|^extraction$|^positive$|^sequencing$", comments$control_type[comments$type == "control"])],
                                       collapse = "\n"), "\nControls control type should be 'pcr', 'extraction', 'positive' or 'sequencing'."))
        )
        
        # nombre d'échantillons
        
        
        if(e1 && e2 && e3 && e4 && e5 && e6 && e7){
          comments_ok(TRUE)
          HTML(paste("Your file is perfect!  <i class='fa-solid fa-heart fa-bounce' style='--fa-animation-iteration-count: 3;'></i><br/>",
                     "You have ",
                     nrow(subset(comments, type == "sample")),
                     " samples in this file.",
                     sep=""))
        }
      })
      
      output$comments_table <- renderTable({
        req(file1)
        read.csv(file1$datapath, sep = "\t", header = T, stringsAsFactors = F, na.strings = "")
      })
    })
    
    # check plates design ----
    observeEvent(input$submit2, {
      
      output$text2 <- renderText({
        file2 <- input$platedesign
        
        validate(
          need(file2, 'Please upload a file!')
        )
        
        ext <- tools::file_ext(file2$datapath)
        validate(
          need(expr = ext %in% c("txt", "tsv", "tab"),
               message = "Please upload a txt, tsv or tab file")
        )
        
        platedesign = read.table(file = file2$datapath, sep = "\t", header = F, stringsAsFactors = F, na.strings = "", check.names = FALSE)
        
        validate(
          need(expr = str_length(input$unused_well) > 0,
               message = "Please enter the word designating unused wells (enter any word if all wells are used)")
        )
        
        unused = input$unused_well
        
        # 1. check sample names
        e1 = all(grepl("^[A-Za-z0-9\\_\\.\\+-]+$", unlist(platedesign)))
        validate(
          need(expr = e1,
               message = paste0("At least one of your sample/control names does not contain authorized characters ([A-Z] [a-z] [0-9] _ - +):\n",
                                paste0(unlist(platedesign)[!grepl("^[A-Za-z0-9\\_\\.\\+-]+$", unlist(platedesign))],
                                       collapse = "\n")))
        )
        
        # 2. duplicated sample names
        e2 = sum(duplicated(unlist(platedesign)[unlist(platedesign) != unused])) == 0
        validate(
          need(expr = e2,
               message = paste0("At least of of your sample/control names are duplicated:\n",
                                paste0(unlist(platedesign)[unlist(platedesign) != unused][duplicated(unlist(platedesign)[unlist(platedesign) != unused])],
                                       collapse = "\n")))
        )
        
        if(e1 && e2){
          platedesign_ok(TRUE)
          HTML(paste("Your file is perfect!  <i class='fa-solid fa-heart fa-beat' style='--fa-animation-iteration-count: 3;'></i><br/>",
                     "",
                     sep=""))
        }
      })
      
      output$platedesign_table <- renderTable({
        req(file2)
        read.csv(file2$datapath, sep = "\t", header = F, stringsAsFactors = F, na.strings = "")
      })
    })
    
    # check tags design ----
    observeEvent(input$submit3, {
      
      output$text3 <- renderText({
        file3 <- input$tags
        
        validate(
          need(file3, 'Please upload a file!')
        )
        
        ext <- tools::file_ext(file3$datapath)
        validate(
          need(ext == "xlsx",
               "Please upload a txt, tsv or tab file")
        )
        
        tags <- readxl::read_xlsx(file3$datapath)
        tags <- as.data.frame(tags)
        rownames(tags) <- tags[,1]
        tags[,1] <- NULL
        
        e1 = var(str_length(sapply(strsplit(unlist(tags),":"), `[`, 1))) == 0
        e2 = var(str_length(sapply(strsplit(unlist(tags),":"), `[`, 2))) == 0
        
        # check forward tags length
        validate(
          need(expr = e1,
               message = "All forward tags must have the same length!\nPlease check the tags in your file.")
        )
        
        # check reverse tags length
        validate(
          need(expr = e2,
               message = "All reverse tags must have the same length!\nPlease check the tags in your file.")
        )
        
        if(e1 && e2){
          tags_ok(TRUE)
          HTML(paste("Your file is perfect!  <i class='fa-solid fa-heart fa-spin' style='--fa-animation-iteration-count: 3;'></i><br/>",
                     "",
                     sep=""))
        }
        
        
      })
      
    })
    
    # check consistency ----
    observeEvent(input$submit4, {
      
      output$text4 <- renderText({
        file1 <- input$comments
        file2 <- input$platedesign
        file3 <- input$tags
        
        # check whether all files are submitted and pass all checks
        validate(
          need(comments_ok() & platedesign_ok() & tags_ok(), "All files must be uploaded and pass all checks.")
        )
        
        comments = read.table(file = file1$datapath, sep = "\t", header = T, stringsAsFactors = F, na.strings = "", check.names = FALSE)
        
        platedesign = read.table(file = file2$datapath, sep = "\t", header = F, stringsAsFactors = F, na.strings = "", check.names = FALSE)
        
        tags <- readxl::read_xlsx(file3$datapath)
        tags <- as.data.frame(tags)
        rownames(tags) <- tags[,1]
        tags[,1] <- NULL
        
        unused = input$unused_well
        
        # check if sample and control names correspond
        e1 = all(sort(unlist(platedesign)[grep(paste0("^", unused, "$"), unlist(platedesign), invert = TRUE)]) == sort(comments$id))
        
        missing_in_comments = ifelse(
          length(
            setdiff(
              sort(unlist(platedesign)[grep(paste0("^", unused, "$"), unlist(platedesign), invert = TRUE)]),
              sort(comments$id))) > 0,
          paste0("Missing in comments file: ", setdiff(sort(unlist(platedesign)[grep(paste0("^", unused, "$"), unlist(platedesign), invert = TRUE)]), sort(comments$id))),
          "Nothing missing in comments file")
        
        missing_in_plates = ifelse(
          length(setdiff(
            sort(comments$id),
            sort(unlist(platedesign)[grep(paste0("^", unused, "$"), unlist(platedesign), invert = TRUE)]))) > 0,
          paste0("Missing in plates design file: ", setdiff(sort(comments$id), sort(unlist(platedesign)[grep(paste0("^", unused, "$"), unlist(platedesign), invert = TRUE)]))),
          "Nothing missing in plates design file")
        
        validate(
          need(expr = e1,
               message = HTML(paste("Samples and controls names don't match between the comments file and the plates design file.\n",
                                    missing_in_comments,
                                    "\n",
                                    missing_in_plates,
                                    sep=""))
          )
        )
        
        
        # check if number of wells in plate design is equal to number of wells in tags design
        #length(unlist(platedesign)) == length(unlist(tags))
        e2 = ncol(platedesign) == ncol(tags)
        
        validate(
          need(expr = e2,
               message = "The plates design and tags design must have the same number of columns.")
        )
        
        e3 = nrow(platedesign) == nrow(tags)
        
        validate(
          need(expr = e3,
               message = "The plates design and tags design must have the same number of rows")
        )
        
        if(e1 && e2 && e3){
          HTML("Your files are consistent!  <i class='fa-solid fa-face-smile fa-bounce' style='--fa-animation-iteration-count: 3;'></i><br/>You can now make your ngsfilter file.")
        }
        
      })
    })
    
    # primers table ----
    output$table <- renderTable({
      colnames(primers) = c("Primer name", "Target taxon", "Forward primer", "Reverse primer", "Target gene", "Reference")
      primers
    })
    
    # make ngsfilter ----
    observeEvent(input$make_ngsfilter, {
      file1 <- input$comments
      file2 <- input$platedesign
      file3 <- input$tags
      
      output$text5 <- renderText({
        e1 = comments_ok() & platedesign_ok() & tags_ok()
        e2 = str_length(input$experiment_name) > 0
        e3 = str_length(input$output_file_name) > 0
        e4 = str_length(input$fwd_primer) > 0 & str_length(input$rev_primer) > 0
        e5 = grepl("^[ACGTURYMKWSBDHVN]+$", input$fwd_primer)
        e6 = grepl("^[ACGTURYMKWSBDHVN]+$", input$rev_primer)
        
        # check whether all files are submitted and pass all checks
        validate(
          need(expr = e1,
               message = "All files must be uploaded and pass all checks (tab 'Check files').")
        )
        
        # check whether an experiment name is given
        validate(
          need(expr = e2,
               message = "Please type an experiment name.")
        )
        
        # check whether a file name is given
        validate(
          need(expr = e3,
               message = "Please type a file name.")
        )
        
        
        # check whether primers are given
        validate(
          need(expr = e4,
               message = "Please type the primers sequences.")
        )
        
        # check whether primers contain IUPAC symbols
        validate(
          need(expr = e5,
               message = "The forward primer must contain nucleic IUPAC symbols.")
        )
        
        validate(
          need(expr = e6,
               message = "The reverse primer must contain nucleic IUPAC symbols.")
        )
        
        if(e1 && e2 && e3 && e4 && e5 && e6){
          HTML("All mandatory fields are correctly filled in, now you can download your ngsfilter file!")
        }
        
      })
      
      
      output$downloadData <- downloadHandler(
        
        filename = input$output_file_name,
        
        content = function(file) {
          comments = read.table(file = file1$datapath, sep = "\t", header = T, stringsAsFactors = F, na.strings = "", check.names = FALSE)
          
          platedesign = read.table(file = file2$datapath, sep = "\t", header = F, stringsAsFactors = F, na.strings = "", check.names = FALSE)
          
          tags <- readxl::read_xlsx(file3$datapath)
          tags <- as.data.frame(tags)
          rownames(tags) <- tags[,1]
          tags[,1] <- NULL
          
          unused = input$unused_well
          
          # if(input$to_add == ""){
          #   print("bla")
            toadd = input$to_add
          # }
          
          
          primerF = rep(input$fwd_primer, ncol(platedesign)*nrow(platedesign))
          primerR = rep(input$rev_primer, ncol(platedesign)*nrow(platedesign))
          Exp = rep(input$experiment_name, ncol(platedesign)*nrow(platedesign))
          
          # print(primerF)
          # print(primerR)
          # print(Exp)
          
          ID = unname(unlist(platedesign))
          
          # number unused pcrs
          n = 1
          for(k in 1:length(ID)){
            if(ID[k] == unused){
              ID[k] = paste0(ID[k], "_", n)
              n = n+1
            }
          }
          
          plcoro = expand.grid(rep(LETTERS[1:8], 4), sprintf("%02d", rep(1:12, 3)))
          colnames(plcoro) = c("row", "col")
          plcoro$rownum = rep(1:32, times = 4)
          plcoro$colnum = rep(1:36, each = 32)
          plcoro$plaque = NA
          
          for(i in 1:nrow(plcoro)){
            if(plcoro$colnum[i] <= 12){
              if(plcoro$rownum[i] <= 8){
                plcoro$plaque[i] = "01"
              }
              if(plcoro$rownum[i] > 8 && plcoro$rownum[i] <=16){
                plcoro$plaque[i] = "02"
              }
              if(plcoro$rownum[i] > 16 && plcoro$rownum[i] <=24){
                plcoro$plaque[i] = "03"
              }
              if(plcoro$rownum[i] > 24){
                plcoro$plaque[i] = "04"
              }
            }
            if(plcoro$colnum[i] > 12 && plcoro$colnum[i] <= 24){
              if(plcoro$rownum[i] <= 8){
                plcoro$plaque[i] = "05"
              }
              if(plcoro$rownum[i] > 8 && plcoro$rownum[i] <=16){
                plcoro$plaque[i] = "06"
              }
              if(plcoro$rownum[i] > 16 && plcoro$rownum[i] <=24){
                plcoro$plaque[i] = "07"
              }
              if(plcoro$rownum[i] > 24){
                plcoro$plaque[i] = "08"
              }
            }
            if(plcoro$colnum[i] > 24){
              if(plcoro$rownum[i] <= 8){
                plcoro$plaque[i] = "09"
              }
              if(plcoro$rownum[i] > 8 && plcoro$rownum[i] <=16){
                plcoro$plaque[i] = "10"
              }
              if(plcoro$rownum[i] > 16 && plcoro$rownum[i] <=24){
                plcoro$plaque[i] = "11"
              }
              if(plcoro$rownum[i] > 24){
                plcoro$plaque[i] = "12"
              }
            }
          }
          plcoro$position = paste(plcoro$plaque, "_", plcoro$col, plcoro$row, sep = "")
          
          id = ID[grep(unused, ID)]
          if(length(id) > 0){
            unused_lines = comments[1:length(id),]
            unused_lines[,1:ncol(unused_lines)] = NA
            unused_lines$id = id
            unused_lines$type = rep("control", length(id))
            unused_lines$control_type = rep("sequencing", length(id))
            comments = rbind(comments, unused_lines)
          }
          
          # if(exists("toadd")){
            ID = paste0(ID, toadd)
          # }
          
          #ID = comments$id
          
          comments_ngs = NULL
          for (i in 1:length(ID)){
            if("sample_id" %in% colnames(comments)){
              comments_ngs = c(comments_ngs, paste("id=", ID[i], ";", sep = ""))
            }
            else{
              # if(exists("toadd")){
                comments_ngs = c(comments_ngs, paste("id=", ID[i], ";sample_id=", substring(ID[i], 1, nchar(ID[i])-nchar(toadd)), ";", sep = ""))
              # }
              # else{
              #   comments_ngs = c(comments_ngs, paste("id=", ID[i], ";sample_id=", ID[i], ";", sep = ""))
              # }
            }
            
            for (j in 2:ncol(comments)){
              comments_ngs[i] = paste(comments_ngs[i], colnames(comments)[j], "=", subset(comments, id == substring(ID[i], 1, nchar(ID[i])-nchar(toadd)))[,j], ";", sep="")
            }
          }
          comments_ngs = sub(";control_type=NA", "", comments_ngs)
          
          
          n=1
          for (i in 1:ncol(platedesign)){
            for (j in 1:nrow(platedesign)){
              position = subset(plcoro, rownum == j & colnum == i)$position
              comments_ngs[n] = paste(comments_ngs[n], "pos_tag_F(rows)=", j, ";pos_tag_R(col)=", i, ";position=", position, ";", sep = "")
              n = n+1
            }
          }
          
          #print(primerF)
          #print(primerR)
          #print(unlist(tags))
          
          ngsfilter.tab = c(Exp, ID, unlist(tags), primerF, primerR, paste0(rep("F @ ", length(ID)), comments_ngs))
          ngsfilter.tab = as.data.frame(matrix(ngsfilter.tab, nrow = length(ID), ncol = 6))
          #print(head(ngsfilter.tab))
          
          write.table(ngsfilter.tab, file, sep = '\t', quote = F, row.names = F, col.names = F)
        }
      )
      
      
    }
    )
    
}
  shinyApp(ui = ui, server = server)
}
  
  
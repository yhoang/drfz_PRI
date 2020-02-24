# rm(list = ls())
### server.R
set_config(config(ssl_verifypeer = 0L))
source(file.path(App.path,"www/helpers.R"))

# Maximum size for single uploaded file is 5MB by default. With the following command
# it is set to 1000MB.
options(shiny.maxRequestSize = 1000*1024^2)

# Needed for marker name trimming in Plotting Data -> Search Markers. Removes all symbols from names.
trimSearch = function(x) {
  change = gsub("[^[:alnum:]]", "", x)
  gsub("[[:space:]]", "", change)
}

# Same function for manual shortname naming except without eval().
trimManShortname = function(x) {
  change = gsub(" ", ".", x, fixed = TRUE)
  change = gsub("^\\*", ".", change)
  change = gsub("-", "_", change)
  change = gsub("[^[:alnum:][:blank:]+?&/\\_\\.]", "", change)
  #change = gsub("[^[:alnum:]+_.-]", " ", change)
  change = sub("\\s+$", "", change)
  change = gsub("[[:space:]]", ".", change)
  gsub("\\.+", ".", change)
}

# Gets all projects of database.
DBprojects = function(conn){
  # Gets all projects within database.
  tables = dbListTables(conn)
  # Excludes all tables which don't include data files (like classification, identity
  # tables, and so on).
  tables[-c(grep("Identity$", tables), grep("Classification$", tables),
            grep("fileComments$", tables), grep("equipmentInfo$", tables), 
            grep("UserHistory", tables), grep("SPILLcolnames$", tables),
            grep("SPILLmatrix$", tables))]
}

# Rewrites new DB name. Spaces will be removed as well as punctuation aside from "+;-".
trimDBName = function(x) {
  change = gsub("[^[:alnum:]+;-]", "", x)
  gsub("[[:space:]]", "", change)
}

# Function which removes all punctuations except "_" and replaces white spaces with "_".
# Furthermore consecutive underscores are replaced by a single one.
trimName = function(x) {
  change = gsub("[^[:alnum:]_]", "_", x)
  change = gsub("[[:space:]]", "_", change)
  change = gsub("_+", "_", change)
  change = sub("_$", "", change)
}

shinyServer(function(input, output, session) {
  # Hide the loading message when the rest of the server function has executed
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
  shinyjs::show("app-content")
  
  disable("files")
  disable("commentFiles")
  disable("classificationButton")
  # Reactive values will be stored in the following object. Serves as an environment.
  values <- reactiveValues()
  
  # Opens pop up for user name with app start.
  toggleModal(session, "login", toggle = "open")
  
  # Saves user's initials and closes pop up.
  observeEvent(input$closeLOGIN, ignoreNULL = TRUE, {
    values$userLogin <- trimDBName(as.character(input$username))
    toggleModal(session, "login", toggle = "close")
  })
  
  # 'Run' button is disabled in the beginning. It will be enabled as soon as all needed 
  # variables for the project are provided.
  disable("updateProject")
  # 'Marker Names'-button is enabled when fcs-files are selected.
  disable("decision")
  # Hides output until compiling of marker names is finished.
  hide("manualBox")
  # Hides discard box until user generated table.
  hide("manDiscardBox")
  
  # Information about table download.
  addTooltip(session, id = "downloadTable",
             title = "To download a table it has to be selected below first.", placement = "right")
  # Gives information about 'Project Information'-button if hovered over.
  addTooltip(session, id = "classButton",
             title = "Button opens window which allows to enter project information. Not required for 
             data upload.", 
             placement = "left")
  # Creates tooltip for further information on new project textInput().
  addTooltip(session, id = "infoCircle", 
             title = "Special characters and white- spaces will be replaced with underscores  
             because databases have difficulties with most punctuations.", placement = "left")
  # Creates tooltip for further information on radioButtons() of compensation question.
  addTooltip(session, id = "compInfoCircle", 
             title = "If compensation matrix is available the uploaded files will be compensated.", 
             placement = "left")
  # Hover text tells user which punctuation is accepted in DB naming.
  addTooltip(session, "initialExperimenter", title = "Accepted punctuation: - + ;")
  addTooltip(session, "dbContent", title = "Accepted punctuation: - + ;")
  addTooltip(session, "addInformation", title = "Accepted punctuation: - + ;")
  
  
  
  output$userpanel <- renderText({
    paste0(" Logged in as ", input$username)
  })
  
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##### TAB: UPLOADING DATA #####
  ##### CREATE NEW/LOAD EXISTING DATABASE #####
  # Loading database if 'Load'-button is clicked.
  observeEvent(input$load, ignoreNULL = TRUE, {
    # Hides 'load'New'-database widgets if user chose that before.
    hide("widgetsNewDB")
    hide("databaseDecision")
    shinyjs::show("restartWidgets")
    shinyjs::show("widgetsLoadDB")
  })
  
  observeEvent(input$dbSelection, ignoreNULL = TRUE, {
    # If no database was selected nothing happens.
    if(nchar(input$dbSelection) == 0){
      return(NULL)
    }else{
      # Otherwise a database connection is established.
      #### PATH TO LOAD DATABASE ####
      DB <<- dbConnect(SQLite(), dbname = paste(DB.path, 
                                                input$dbSelection, ".sqlite3", sep = ""))
      shinyjs::show("choiceProject")
      updateSelectizeInput(session, "choiceProject", label = "Extend a project or create a new one?", 
                           choices = list("Load", "New"),
                           options = list(placeholder = NULL, 
                                          onInitialize = I('function() {this.setValue(""); }')))
    }
  })
  
  # Compiling database name if 'New'-button is clicked.
  observeEvent(input$new, ignoreNULL = TRUE, {
    # Disables 'Load'- and 'New'-button to prevent double-click.
    disable("load")
    disable("new")
  })
  
  # If 'Compile Name'-button is clicked input is merged to one character string.
  observeEvent(input$saveDB, ignoreNULL = TRUE, {
    if(any(nchar(c(input$initialExperimenter, input$dbContent)) == 0)){
      toggleModal(session, "modalDBWarning", "open")
      output$DBWarning <- renderText({
        paste("Please provide input in all text fields marked with an asterik.")
      })
    }else{
      # If no additional information is provided, input won't be used in naming.
      if(nchar(input$addInformation) > 0){
        nameDB = paste(trimDBName(input$initialExperimenter), "_",
                       gsub("-", "", input$measureDate), "_",
                       trimDBName(input$dbContent), "_", 
                       trimDBName(input$addInformation), sep = "")
      }else{
        nameDB = paste(trimDBName(input$initialExperimenter), "_",
                       gsub("-", "", input$measureDate), "_",
                       trimDBName(input$dbContent), sep = "")
      }
      
      # Stores DB name in reactiveValues().
      values$dbName <- paste0(nameDB, ".sqlite3")
      
      if(file.exists(paste0(DB.path, values$dbName))){
        toggleModal(session, "modalDBWarning", "open")
        output$DBWarning <- renderText({
          paste("This database exists already. Please choose another filename.")
        })
      }else{
        hide("cancelDB")
        shinyjs::show("closeDB")
        
        # Display of current name.
        output$infoDBName <- renderUI({
          helpText("The filename can be edited as long as this window is open. The current
                   database name is:")
        })
        
        output$newDBName <- renderText({
          if(!is.null(values$dbName)) {
            values$dbName
          }
        })
        }
    }
})
  
  observeEvent(input$closeDBWarning, ignoreNULL = TRUE, {
    toggleModal(session, "modalDBWarning", "close")
  })
  
  # 'Name New Database'-pop up will be closed.
  observeEvent(input$cancelDB, ignoreNULL = TRUE, {
    # Enables both buttons again.
    enable("load")
    enable("new")
    toggleModal(session, "newDBWindow", toggle = "close")
  })
  
  # 'Name New Database'-pop up will be closed and connection to new DB established.
  observeEvent(input$closeDB, ignoreNULL = TRUE, {
    hide("databaseDecision")
    # Shows new database widgets.
    toggle("widgetsNewDB")
    shinyjs::show("restartWidgets")
    DB <<- dbConnect(SQLite(), dbname = paste0(DB.path, values$dbName))
    toggleModal(session, "newDBWindow", toggle = "close")
  })
  
  observeEvent(input$restartButton, ignoreNULL = TRUE, {
    # Disconnects eventual database connections.
    if(exists("DB")) {dbDisconnect(DB)}
    js$refresh()
  })
  
  # No matter if connection to new or existing database was established, a widget to load or create
  # a new project will appear.
  #c(input$load, input$new)
  observeEvent(input$choiceProject, ignoreNULL = TRUE, {
    # Allows selection of project available in loaded database.
    output$projectChoice <- renderUI({
      if(input$choiceProject == "New" || input$new){
        textInput("project", label = NULL, placeholder = "e.g. Project_1")
      }else{
        if(input$choiceProject == "Load"){
          validate(need(length(dbListTables(DB)) > 0, "Database is empty. No projects exist."))
          selectInput("project", label = NULL, choices = DBprojects(DB))
        }
      }
    })
  })
  
  # Leading numbers in DB table names do not work.
  observeEvent(input$project, ignoreNULL = TRUE, {
    if(nchar(input$project) > 0){
      if(grepl("^[[:digit:]]", input$project)){
        toggleModal(session, "warningProject", "open")
      }else{
        enable("files")
        enable("commentFiles")
        enable("classificationButton")
      }
    }else{
      disable("files")
      disable("commentFiles")
      disable("classificationButton")
    }
  })
  
  # Closes input$project warning and sets it back to default.
  observeEvent(input$closeProject, ignoreNULL = TRUE, {
    updateTextInput(session, "project", label = NULL, value = "")
    toggleModal(session, "warningProject", "close")
  })
  
  # Function to remove invalid characters from projectname.
  projectname <- reactive({
    if(input$choiceProject == "New" || input$new){
      if(grepl("[[:alnum:]]", input$project) || grepl("[[:space:]]", input$project)){
        updateTextInput(session, "project", label = NULL, value = trimName(input$project))
      }
    }
    string = as.character(input$project)  
  })
  
  
  ##### CLASSIFICATION TABLE #####
  # Modal window shows up when classification button is clicked.
  observeEvent(input$classificationButton, ignoreNULL = TRUE, {
    # If no table exists pop up to create one opens.
    if(!dbExistsTable(DB, paste(projectname(), "_Classification", sep = ""))){
      toggleModal(session, "classificationWindow", "open")
    }else{
      # Otherwise user is told the table exists already and nothing else happens.
      toggleModal(session, "classTableExists", "open")
      # Note that table already exists.
      output$noteClassTable <- renderText({
        paste("Project information were already given.")
      })
    }
  })
  
  # Closes 'classTable exists already'-pop up.
  observeEvent(input$note, ignoreNULL = TRUE, {
    toggleModal(session, "classTableExists", "close")
  })
  
  # Renders classification table.
  observeEvent(input$addButton1, ignoreNULL = TRUE, {
    # If user does not provide comments a variable containing 'none' is compiled.
    comments = ifelse(nchar(input$comments) == 0, "none", input$comments)
    
    # If user does not provide gender a variable containing 'not provided' is compiled.
    gender = ifelse(nchar(input$gender) == 0, "not provided", input$gender)
    
    # Vector with all possible values.
    info = c(Project = as.character(input$project), "Start of Project" = as.character(input$date), 
             Experimenter = input$experimenter, "Type of Experiment" = input$expType, 
             Species = input$species, Subspecies = input$subspecies, Gender = gender, 
             Tissue = input$tissue, Celltype = input$celltype, 
             Subpopulation = input$subpopulation, 
             Stimulation = input$stimulation, 
             "Duration of Stimulation" = input$duration,
             "Staining_Information" = input$staining, Comments = comments)
    
    # Warning message appears if values are missing.
    if(any(nchar(info) == 0)){
      missing = names(which(nchar(info) == 0))
      toggleModal(session, "modalClassWarning", "open")
      output$classWarning <- renderText({
        paste("Please provide information for the following fields: ", 
              paste(missing, collapse = " & "), ".", sep = "")
      })
    }else{
      # Function which compiles temporary table.
      tempClass = data.frame(Categories = names(info), Description = info)
      
      # Shows compiled classification table.
      output$classification <- renderDataTable({
        # Overwrite allowed if user wants to change an entry before closing pop up.
        dbWriteTable(DB, paste(projectname(), "_Classification", sep = ""), tempClass, 
                     overwrite = TRUE)
        tempClass
      })
      
      # Note for 'Close'-button.
      output$helpText <- renderUI({
        helpText(HTML("<p align='right'><b>Note:</b> Please check the information before 
                      <br> you close the window and save it.</p>"))
      })
      }
    })
  
  # Creates 'Close'-button for pop up as soon as table is compiled.
  observeEvent(input$closeClassification, ignoreNULL = TRUE, {
    toggleModal(session, "classificationWindow", "close")
    
    # Locks projectname.
    disable("project") 
  })
  
  
  ##### UPLOAD FILES FOR FILEIDENTITY TABLE #####
  # If 'Select files'-button is pressed, folder window opens to choose files. Paths
  # of files will be saved. Displays .fcs files first but leaves possibility to select
  # 'All files' (sometimes file type is not set to .fcs even if it is...).
  observeEvent(input$files, ignoreNULL = TRUE, {
    # Disables 'Select Files'- and 'Comment Files'-button to prevent double-click or crash of app.
    disable("files")
    disable("commentFiles")
    values$file.paths <- NULL
    # Removes markerNames entry from reactiveValues().
    values$markerNames <- NULL
    
    filter = matrix(c("All files", "*", "FACS", "fcs"), 2, 2, byrow = TRUE)
    file.paths = jchoose.files(caption = "Select file(s)", filters = filter, multi = TRUE)
    
    # To make sure the app does not crash, nothing will happen if user cancels file selection.
    if(length(file.paths) == 0) {
      enable("files")
    }else{
      # Resets 'chosenBox' tables.
      output$chosenTable <- renderTable({return()})
      output$excFiles <- renderText({return()})
      output$excTable <- renderTable({return()})
      output$diffFiles <- renderText({return()})
      output$diffTable <- renderTable({return()})
      output$oddFiles <- renderText({return()})
      output$oddTable <- renderTable({return()})
      # Clear warning text from 'Marker Table'-pop up.
      output$diffFilesText <- renderText({return()})
      output$oddFilesText <- renderText({return()})
      
      
      # Set working directory to path of chosen files.
      setwd(dirname(file.paths[1]))
      
      # Control if R read the filename correctly.
      if(any(file.exists(file.paths) == FALSE) | any(grepl("[[:space:]]", basename(file.paths)))) {
        shinyjs::hide("chosenBox")
        toggleModal(session, "invalidFilenames", toggle = "open")
        
        if(any(file.exists(file.paths) == FALSE)){
          # Saves files which paths could not be read.
          invalid = file.paths[which(file.exists(file.paths) == FALSE)]
          values$file.paths <- NULL
          
          # Displays how R read the files...
          output$invalidFiles = renderText({
            paste("The program could not load the files. Please check your filenames or folders for 
                  invalid characters and change those. Please keep in mind to not use special characters. 
                  The following displays how the program read the invalid path:")
          })
          # ...in the following table.
          output$invalidTable <- renderTable({
            table = NULL
            for(i in 1:length(invalid)){
              table = rbind(table, invalid[i])
            }
            colnames(table) = "Invalid Files"
            table
          }, include.rownames = FALSE, striped = TRUE, include.colnames = FALSE)
          
          output$summaryFileIdentity <- renderText({
            paste("The uploaded filename could not be read. Please revise your upload files.")
          })
          values$file.paths <- NULL
          }else{
            space = file.paths[which(grepl("[[:space:]]", basename(file.paths))==TRUE)]
            values$file.paths <- NULL
            
            # Displays how R read the files...
            output$invalidFiles = renderText({
              paste("Please make sure that your filenames do not contain any whitespace. The following 
                    displays the filenames to be edited:")
            })
            # ...in the following table.
            output$invalidTable <- renderTable({
              table = NULL
              for(i in 1:length(space)){
                table = rbind(table, basename(space[i]))
              }
              colnames(table) = "Files with whitespace"
              table
            }, include.rownames = FALSE, striped = TRUE, include.colnames = FALSE)
            }
        }else{
          # Calculates 32-byte md5 hashes for all loaded files. Used to determine whether files
          # were already uploaded into project or not.
          withProgress(message = "", value = 0, {
            md5_hash = NULL
            for(i in 1:length(file.paths)) {
              ##### PROGRESS MESSAGE      
              incProgress(1/length(file.paths), message = "", detail = "Checking if files are 
                          already included in project. This may take a while.")
              ##### PROGRESS MESSAGE
              
              md5_hash = c(md5_hash, digest(file.paths[i], algo = "md5", file = TRUE))
            }
          })
          
          shinyjs::show("chosenBox")
          
          # If files were read correctly they will be either compared to files in project_fileIdentity or
          # not - depending on if such table exists or not.
          if(!dbExistsTable(DB, paste(projectname(), "_fileIdentity", sep = ""))) {
            # If fileIdentity table does not exist no files are in the project -> all selected 
            # files can be uploaded. The following displays files the user selected.
            output$summaryFileIdentity <- renderText({
              paste("You uploaded ", length(file.paths), " file(s). See below for further details.",
                    sep = "")
            })
          }else{
            # Get fileTable from database.
            sql.fileIdentity = paste("select * from ", projectname(), "_fileIdentity", sep = "")
            fileTable = dbGetQuery(DB, sql.fileIdentity)
            
            # Check if files are already in fileIdentity table.
            if(any(md5_hash %in% fileTable[, "md5_hash"]) == TRUE) {
              # If md5_hash already exists in fileTable the position of the duplicate will be
              # determined. 
              file = which(md5_hash %in% fileTable[, "md5_hash"] == TRUE)
              # ...and deleted from file list + md5_hash varaiable.
              exc.files = file.paths[file]
              exc.md5 = md5_hash[file]
              file.paths = file.paths[-file]
              md5_hash = md5_hash[-file]
            }
            
            # Depending on result of comparison of existing md5_hash and the ones of loaded 
            # files the appropriate text is displayed
            output$summaryFileIdentity <- renderText({
              if(length(file.paths) == 0 ) {
                paste("No file was uploaded. The chosen files were either already included in
                      the project or no files were selected.")
              }else{
                if(exists("exc.files")) {
                  paste("You uploaded ", length(file.paths), " file(s). ", length(exc.files),
                        " file(s) was/were excluded due to being double. See below for further 
                        details.", sep = "")
                }else{
                  paste("You uploaded ", length(file.paths), " file(s). See below for further 
                        details.", sep = "")
                }
                }
              })
            }
          
          if(length(file.paths) != 0) {
            output$summaryFiles <- renderText({
              paste("The file(s) to be included in the project are:", sep = "")
            })
            
            # Display chosen files.
            output$chosenTable <- renderTable({
              half = ceiling(length(file.paths)/2)
              col1 = basename(file.paths[1:half])
              
              if(length(file.paths) > 1) {
                col2 = basename(file.paths[(half+1):length(file.paths)])
                
                if(length(col2) < length(col1)){
                  col2 = c(col2, "")
                }
                data.frame(col1, col2)
              }else{
                data.frame(col1)
              }
            }, include.rownames = FALSE, striped = TRUE, include.colnames = FALSE)
            
            if(exists("exc.files")){
              output$excFiles <- renderText({
                paste("The following file(s) was/were excluded due to being double: ")
              })
              
              output$excTable <- renderTable({
                data.frame("file_ID from Table" = fileTable[which(fileTable[, "md5_hash"]%in%exc.md5 == TRUE), 
                                                            "file_ID"], 
                           "Uploaded Filename" = basename(exc.files))
              }, striped = TRUE)
            }
          }else{
            output$summaryFiles <- renderText({
              paste("No file was uploaded.", sep = "")
            })
          }
          
          # Save actual file paths, md5_hash and filename in reactiveValues().
          values$file.paths = file.paths
          values$md5_hash = md5_hash
          values$filename = basename(file.paths)
          
          if(length(values$file.paths) != 0){
            # Button for marker table settings will be enabled if files are selected.
            enable("decision")
            # Locks projectname.
            disable("project")
          }
            }
      
        }
    
    ### Needed if all steps are performed several times.
    # Clears textOutput in 'Include Data'-box.
    output$summaryProject <- renderText({return()})
    output$UserHistory <- renderDataTable({return()})
    output$markerTable <- renderTable({return()})
    output$markerNamesTable <- renderTable({return()})
    output$shortnameInput <- renderUI({return()})
    
    # Enables buttons again.
    enable("files")
    enable("commentFiles")
    })
  
  
  ##### ENTRIES FOR FILECOMMENTS TABLE #####
  # Renders checkbox-UI for file selection.
  observeEvent(input$commentFiles, ignoreNULL = TRUE, {
    # enable("project")
    
    # Checkbox which shows all selected files.
    output$checkboxFiles <- renderUI({
      # If project_fileIdentity exists, files to upload and files listed in project are displayed.
      # Thus files can be commented anytime.
      if(dbExistsTable(DB, paste(projectname(), "_fileIdentity", sep = ""))) {
        fileTable = dbReadTable(DB, paste(projectname(), "_fileIdentity", sep = ""))
        filenames = fileTable[, "filename"]
        # 'unique' in case user decides to comment files after uploading them. After upload
        # the filenames are still in reactiveValues() and will be displayed along the
        # ones already in projectname_fileIdentity.
        filenames = unique(c(values$filename, filenames))
        checkboxGroupInput("chooseFiles", "Which files would you like to comment?", 
                           choices = filenames)
      }else{
        checkboxGroupInput("chooseFiles", "Which files would you like to comment?", 
                           choices = values$filename)
      }
    })
    
    # Locks projectname.
    disable("project")
  })
  
  # If 'Show table'-button is clicked the provided informationen is added to the 
  # project_fileComments table. It is only updated if button is pressed.
  observeEvent(input$addButton3, ignoreNULL = TRUE, {
    if(!is.null(input$chooseFiles) && nchar(input$fileComments)!=0){
      # Temporary table with chosen files and comments.
      commentTable = data.frame() 
      for(i in 1:length(input$chooseFiles)) {
        commentFiles = input$chooseFiles
        commentTable[i, 1] = commentFiles[i]
        commentTable[i, 2] = input$fileComments
      }
      # Change of colnames and saving of table in database.
      colnames(commentTable) = c("filename", "comments")
      
      values$commentTable <- commentTable
      
      output$fileCommentsTable <- renderDataTable({
        values$commentTable
      })
      hide("cancelComment")
    }
  })
  
  # If 'Remove entry'-button the entries of the selected files with the current comment 
  # are removed from the project_fileComments table. Table is only updated if button is 
  # pressed.
  observeEvent(input$removeComment, ignoreNULL = TRUE, {
    if(!is.null(input$chooseFiles) && nchar(input$fileComments)!=0){
      # Removing rows that correspond to chosen files and comment. Hereby nothing 
      # changes for other entries.
      commentTable = values$commentTable
      commentTable = commentTable[!(commentTable$filename %in% input$chooseFiles & 
                                      commentTable$comments %in% input$fileComments), ]
      
      values$commentTable <- commentTable
      
      output$fileCommentsTable <- renderDataTable({
        values$commentTable
      })
    }
  })
  
  # Displays close button of modal window.
  observeEvent(input$addButton3, ignoreNULL = TRUE, {
    # Note for 'Close'-button.
    output$helpTextComments <- renderUI({
      helpText(HTML("<p align='right'><b>Note:</b> Please check the information before 
                    <br> you close the window and save it.</p>"))
    })
    })
  
  # Closes 'Comment files'-pop up.
  observeEvent(input$closeComments, ignoreNULL = TRUE, {
    toggleModal(session, "commentsWindow", "close")
  })
  
  observeEvent(input$cancelComment, ignoreNULL = TRUE, {
    toggleModal(session, "commentsWindow", "close")
  })
  
  ##### UPLOAD FILE FOR MARKERIDENTITY TABLE #####
  # TextInput()'s depending on ncol() of uploaded files are generated and user can enter shortnames or select 
  # the ones already included in project_markerIdentity["shortname"].
  observeEvent(input$decision, ignoreNULL = TRUE, {
    output$diffFilesText <- renderText({return()})
    output$oddFilesText <- renderText({return()})
    hide("manualBox")
    hide("manDiscardBox")
    
    values$projectname = as.character(input$project)
    
    # Get column number of all chosen files.
    listCOL = list()
    colNum = NULL
    for(i in 1:length(values$file.paths)){
      fileFCS = read.FCS(values$file.paths[i], which.lines = 1,transformation=F)
      colNum = c(colNum, ncol(fileFCS))
      name = paste0("C", i)
      listCOL[[name]] <- str_replace_all(colnames(fileFCS), "[^[:alnum:]]", "")
    }
    
    # If column quantity differs, files distinct from first one are removed. The rest can be uploaded.
    if(!all(colNum == colNum[1])){
      # Get name of files with different column quantity...
      diff.files = basename(values$file.paths[which(colNum != colNum[1])])
      # ...and remove those from variables for upload.
      values$file.paths = values$file.paths[-which(colNum != colNum[1])]
      values$md5_hash = values$md5_hash[-which(colNum != colNum[1])]
      values$filename = values$filename[-which(colNum != colNum[1])]
      listCOL[[which(colNum != colNum[1])]] <- NULL
      
      # Inform user about removed files.
      output$diffFilesText <- renderText({
        paste0("The following file(s) differ in column quantity compared to FIRST UPLOADED FILE and are excluded 
               in further computations. Please make sure that files uploaded in one session show same staining 
               information and same number of fluorescence channels. Removed: ", 
               paste0(diff.files, collapse = ", "))
      })
      
      ### Displays excluded files in 'Chosen Files'-box.
      # Text
      output$diffFiles <- renderText({
        paste("The following file(s) was/were excluded due to differing column quantity: ")
      })
      # Table
      output$diffTable <- renderTable({
        half = ceiling(length(diff.files)/2)
        col1 = basename(diff.files[1:half])
        # Sorts filenames in dataframe.
        if(length(diff.files) > 1) {
          col2 = basename(diff.files[(half+1):length(diff.files)])
          
          if(length(col2) < length(col1)){
            col2 = c(col2, "")
          }
          data.frame(col1, col2)
        }else{
          data.frame(col1)
        }
      }, include.rownames = FALSE, striped = TRUE, include.colnames = FALSE)
      
      # Update information of 'Select Files'.
      output$summaryFileIdentity <- renderText({
        paste(length(values$file.paths), " files are ready for uploading. See below for further details.", sep = "")
      })
      # Update table of selected files in 'Chosen Files'-box.
      output$chosenTable <- renderTable({
        half = ceiling(length(values$filename)/2)
        col1 = values$filename[1:half]
        
        if(length(values$filename) > 1) {
          col2 = values$filename[(half+1):length(values$filename)]
          
          if(length(col2) < length(col1)){
            col2 = c(col2, "")
          }
          data.frame(col1, col2)
        }else{
          data.frame(col1)
        }
      }, include.rownames = FALSE, striped = TRUE, include.colnames = FALSE)
    }
    
    # Check if colnames (staining) are equal... 
    noMatch = NULL
    for(entry in 1:length(names(listCOL))) {
      if(!all(listCOL[[1]] %in% listCOL[[entry]])){
        noMatch = c(noMatch, entry)
      }
    }
    # ...and remove files from upload list if they differ from 1st file.
    if(!is.null(noMatch)){
      odd.files = values$filename[noMatch]
      values$file.paths = values$file.paths[-noMatch]
      values$md5_hash = values$md5_hash[-noMatch]
      values$filename = values$filename[-noMatch]
      
      # Inform user about removed files.
      output$oddFilesText <- renderText({
        paste0("The following file(s) show differing marker names compared to FIRST UPLOADED FILE and are 
               excluded in further computations. Please make sure that files uploaded in one session show 
               same staining information and same number of fluorescence channels. Removed: ", 
               paste0(odd.files, collapse = ", "))
      })
      
      ### Displays excluded files in 'Chosen Files'-box.
      # Text
      output$oddFiles <- renderText({
        paste("The following file(s) was/were excluded due to differing staining (file colnames): ")
      })
      # Table
      output$oddTable <- renderTable({
        half = ceiling(length(odd.files)/2)
        col1 = basename(odd.files[1:half])
        # Sorts filenames in dataframe.
        if(length(odd.files) > 1) {
          col2 = basename(odd.files[(half+1):length(odd.files)])
          
          if(length(col2) < length(col1)){
            col2 = c(col2, "")
          }
          data.frame(col1, col2)
        }else{
          data.frame(col1)
        }
      }, include.rownames = FALSE, striped = TRUE, include.colnames = FALSE)
      
      # Update information of 'Select Files'.
      output$summaryFileIdentity <- renderText({
        paste(length(values$file.paths), " files are ready for uploading. See below for further details.", 
              sep = "")
      })
      # Update table of selected files in 'Chosen Files'-box.
      output$chosenTable <- renderTable({
        half = ceiling(length(values$filename)/2)
        col1 = values$filename[1:half]
        
        if(length(values$filename) > 1) {
          col2 = values$filename[(half+1):length(values$filename)]
          
          if(length(col2) < length(col1)){
            col2 = c(col2, "")
          }
          data.frame(col1, col2)
        }else{
          data.frame(col1)
        }
      }, include.rownames = FALSE, striped = TRUE, include.colnames = FALSE)
    }
    
    
    # Calculates quantity of input depending on columns of one of the uploaded files.
    output$shortnameInput <- renderUI({
      # Read one of the uploaded files (same staining information!).
      fcs = read.FCS(values$file.paths[1],transformation=F)
      values$fcsColnames = colnames(fcs)
      
      # If project_markerIdentity does exist the shortname column is loaded. User can select those.
      if(dbExistsTable(DB, paste(values$projectname, "_markerIdentity", sep = ""))) {
        autoShortnames = unique(dbReadTable(DB, paste(values$projectname, "_markerIdentity", sep = ""))
                                [,"shortname"])
      }else{
        autoShortnames = NULL
      }
      # FCS header is saved in key.
      key = keyword(fcs)
      
      if(any(grepl(paste(c("FSC","SSC"), collapse="|"), values$fcsColnames))){
        # Excludes 'FSC' and 'SSC' for next step.
        names = values$fcsColnames[-grep(paste(c("FSC","SSC"), collapse="|"), values$fcsColnames)]
        # Removes all non-alphanumeric characters and sorts names alphabetically. FSC and SSC are excluded.
        names = sort(str_replace_all(names, "[^[:alnum:]]", ""))
        #names = sort()
        # FSC and SSC included again.
        values$sorted = c(str_replace_all(values$fcsColnames[grep(paste(c("FSC","SSC"), collapse="|"), 
                                                                  values$fcsColnames)], "[^[:alnum:]]", ""), names)
      }else{
        values$sorted = sort(str_replace_all(values$fcsColnames, "[^[:alnum:]]", ""))
      }
      values$sorted = make.names(values$sorted,unique=T)
      print(length((values$fcsColnames)))
      
      # Here, quantity of selectizeInput() is compiled.
      temp.colnames = str_replace_all(values$fcsColnames, "[^[:alnum:]]", "")
      temp.colnames = make.names(temp.colnames, unique = T)
      lapply(1:length(values$fcsColnames), function(i) {
        # Gets colname position according to sorted marker names.
        
        #number = which(values$sorted[i] == str_replace_all(values$fcsColnames, "[^[:alnum:]]", ""))
        number = which(values$sorted[i] == temp.colnames)
        
        #print(number)
        
        # Marker names are available from keyword() at $`P..S`.
        keyname = eval(parse(text = paste("key$`$P", number,"S`",sep = "")))
        
        # If there is no entry the corresponding file colname is picked.
        if(keyname == " " || is.null(keyname)) {
          default = temp.colnames[number]
        }else{
          # Following lines remove all punctuations except "+_.-", replace white spaces with "."
          # and delete last white space within string. Furthermore consecutive periods are replaced
          # by a single one.
          change = gsub(" ", ".", keyname, fixed = TRUE)
          change = gsub("[^[:alnum:]+_.-]", " ", change)
          change = sub("\\s+$", "", change)
          change = gsub("[[:space:]]", ".", change)
          default = gsub("\\.+", ".", change)
        }
        # User can select shortnames from drop-down menu or decide on a new one.
        selectizeInput(paste("ID",i, sep=""),paste(temp.colnames[number]), 
                       choices = unique(c(autoShortnames, default)),
                       selected = default, options = list(create = TRUE))
      })
    })
    # Shows output. Needed if user reseted it before.
    shinyjs::show("manualBox")
    })
  
  # Print table: 1st column fcs colnames and 2nd column shortnames. Table is saved in
  # values$markerNames.
  observeEvent(input$showColTable, ignoreNULL= TRUE, {
    shinyjs::show("manDiscardBox")
    
    output$markerTable <- renderTable({
      file_colname = NULL
      shortname = NULL
      temp.colnames = str_replace_all(values$fcsColnames, "[^[:alnum:]]", "")
      temp.colnames = make.names(temp.colnames, unique = T)
      for(i in 1:length(values$fcsColnames)) {
        #number = which(values$sorted[i] == str_replace_all(values$fcsColnames, "[^[:alnum:]]", ""))
        number = which(values$sorted[i] == temp.colnames)
        file_colname = c(file_colname, values$fcsColnames[number])
        shortname = c(shortname, input[[paste0("ID", i)]])
      }
      
      #print(file_colname)
      print(length(shortname))
      
      shortname = sapply(1:length(shortname), function(i){trimManShortname(shortname[i])})
      
      ### if shortnames are doubled
      #if ( length(unique(shortname)) < length(shortname) ) {
      shortname = make.names(shortname, unique = TRUE)
      #}
      
      markerNamesTABLE = data.frame(file_colname, shortname)
      values$markerNames <- data.frame(file_colname, shortname)
    }, striped = TRUE)
  })
  
  # Closes modal window and values$markerNames is kept.
  observeEvent(input$manKeep, ignoreNULL = TRUE, {
    output$markerTable <- renderDataTable({return()})
    output$shortnameInput <- renderUI({return()})
    toggleModal(session, "markerNamesWindow", toggle = "close")
  })
  
  # Closes modal window whithout saving any uploads which allows user to
  # choose other files. Additionally, the former output is cleared -> reset to
  # default modal window.
  observeEvent(input$manDiscard, ignoreNULL = TRUE, {
    values$markerNames <- NULL
    output$markerTable <- renderDataTable({return()})
    output$shortnameInput <- renderUI({return()})
    hide("manualBox")
    hide("manDiscardBox")
    values$helpValue <- NULL
    toggleModal(session, "markerNamesWindow", toggle = "close")
  })
  
  
  ##### USER HISTORY TABLE #####
  # Requests information of current file upload and updates user history table.
  observeEvent(input$addButton2, ignoreNULL = TRUE, {
    if(nchar(input$commentsUser) == 0){
      commentsUser = "none"
    }else{
      commentsUser = input$commentsUser
    }
    # For each uploaded file an entry is generated to delete files later on.
    tempUserTable = data.frame(Date = rep(as.character(Sys.time()), length(values$file.paths)), 
                               User = rep(values$userLogin, length(values$file.paths)),
                               Project = rep(values$projectname, length(values$file.paths)), 
                               File = values$filename,
                               Comments = rep(commentsUser, length(values$file.paths)),
                               stringsAsFactors = FALSE)
    values$tempUserTable <- tempUserTable
    
    # Shows complete user history.
    output$UserHistory <- renderDataTable({
      if(dbExistsTable(DB, "UserHistory")) {
        table = dbReadTable(DB, "UserHistory")
        table = rbind(table, values$tempUserTable)
        table
      } else {
        values$tempUserTable
      }
    })
    shinyjs::show("closeModalUserHistory")
  })
  
  observeEvent(input$closeModalUSER, ignoreNULL = TRUE, {
    dbWriteTable(DB, "UserHistory", values$tempUserTable, append = TRUE)
    # Returns everything within pop up to its original state.
    output$UserHistory <- renderDataTable({return()})
    hide("closeModalUserHistory")
    reset("commentsUser")
    
    # Closes modal pop-up.
    toggleModal(session, "userHistory", toggle = "close")
  })
  
  ##### UPDATE IDENTITYTABLES AND PROJECT TABLE IN DATABASE #####
  # 'Run' button is enabled as soon as file.paths and markerNames are added to reactiveValues().
  observeEvent(c(values$file.paths, input$keep, input$manKeep, input$project), 
               ignoreNULL = TRUE, {
                 if(nchar(input$project) > 0 && !is.null(values$markerNames) && length(values$file.paths) != 0){
                   enable("updateProject")
                 }
               })
  
  includeFiles <- function() {
    #### UPDATE FILECOMMENTS TABLE ####
    if(!is.null(values$commentTable)){
      dbWriteTable(conn = DB, paste(values$projectname, "_fileComments", sep = ""), 
                   values$commentTable, append = TRUE) 
    }
    
    #### UPDATE FILEIDENTITY TABLE ####
    # Handling of staining information input.
    stain = ifelse(nchar(input$stainInfo)==0, "none", input$stainInfo)
    
    if(!dbExistsTable(DB, paste(values$projectname, "_fileIdentity", sep = ""))){
      # File_ID is based on number of uploaded files. Filenames are extracted from file 
      # path. Staining information is given by user. The Date will be added later  on.
      # Combines five variables in one data frame.
      fileTable = data.frame(file_ID = 1:length(values$file.paths), 
                             filename = values$filename, 
                             "Date_of_Measurement" = rep(NA, length(values$file.paths)), 
                             "Staining_Information" = rep(stain, length(values$file.paths)), 
                             md5_hash = values$md5_hash)
      # Define fileIdentity tablename and create new fileTable within database.
      dbWriteTable(DB, paste(values$projectname, "_fileIdentity", sep = ""), fileTable)
    }else{
      # Load fileIdentity table from database.
      sql.fileIdentity = paste("select * from ", values$projectname, "_fileIdentity", sep = "")
      fileTable = dbGetQuery(DB, sql.fileIdentity)
      
      # It determines the highest fileID and compiles new ones accordingly to number of 
      # uploaded files.
      file_ID = (max(fileTable[, "file_ID"]) + 1) : (max(fileTable[, "file_ID"]) + 
                                                       length(values$file.paths))
      # Date will be added later on.
      tempFileTable = data.frame(file_ID, filename = values$filename, 
                                 "Date_of_Measurement" = rep(NA, length(values$file.paths)), 
                                 "Staining_Information" = rep(stain, length(values$file.paths)), 
                                 md5_hash = values$md5_hash)
      # Appends existing fileTable in database.
      dbWriteTable(DB, paste(values$projectname, "_fileIdentity", sep = ""), tempFileTable, 
                   append = TRUE)
    }
    
    #### UPDATE MARKERIDENTITY TABLE ####
    # Creates vector with database colnames based on row number in colname table.
    database_colnames = paste("col", sprintf("%02i", 1:nrow(values$markerNames)), 
                              sep = "")
    
    # Added 'fileIdentity'-table is reloaded.
    sql.fileIdentity = paste("select * from ", values$projectname, "_fileIdentity", sep = "")
    fileTable = dbGetQuery(DB, sql.fileIdentity)
    
    markerTable = NULL
    for(i in 1:length(values$file.paths)) {
      # Determines fileID of all uploaded files. If filename doubles last match is chosen. One ID per 
      # loop. Gets fileID from looking for filename within fileTable. 'Fixed' is used to lock the pattern 
      # so that symbols are simply read as what they are and not interpreted as regular expressions.
      fileID = fileTable[tail(which(fileTable[, "filename"] == values$filename[i]), n = 1), "file_ID"]
      fileID = rep(fileID, length(database_colnames))
      tempMarkerTable = data.frame(file_ID = fileID, 
                                   database_colnames, 
                                   file_colname = values$markerNames[, 1], 
                                   shortname = values$markerNames[, 2])
      markerTable = rbind(markerTable, tempMarkerTable)
    }
    
    # Depending whether markerIdentity exists or not table is either created or 
    # extended.
    dbWriteTable(DB, name = paste(values$projectname, "_markerIdentity", sep = ""), 
                 value = markerTable, append = TRUE)
    
    #### UPDATE PROJECT TABLE ####
    withProgress(message = "", value = 0, {
      if(dbExistsTable(conn = DB, values$projectname)) {
        # If input$project-table exists the index is dropped. The Removal before an insertion
        # is more efficient.
        sql.query = paste("drop index ", values$projectname, "_idxfile", sep = "")
        dbGetQuery(DB, sql.query)
      }
      
      for (i in 1:length(values$file.paths)){
        fileID = fileTable[which(fileTable[, "filename"] == values$filename[i]), 
                           "file_ID"]
        fcsfile = sprintf("%i", fileID)
        fcs = read.FCS(values$file.paths[i],transformation=F)
        
        ##### PROGRESS MESSAGE      
        text = paste("INCLUDING File (index ", fileID, ") ", i, " of ", 
                     length(values$file.paths), " with ", nrow(fcs), " rows and ", 
                     ncol(fcs), " columns is being added to the project ", values$projectname, 
                     " of the database. During the last run the table will be indexed to 
                     allow a faster access later on. This step takes some time.", sep = "")
        incProgress(1/length(values$file.paths), message = "", detail = text)
        ##### PROGRESS MESSAGE
        
        ### Handling of compensation matrix.
        # Get keywords.
        keywords = keyword(fcs)
        
        # Different names given for adjustment matrix. Therefore the header is searched for matrices. If
        # there none adjustments will be made - newer FCS versions keep matrix in fcs-header even after adjustment
        # was done... It will be removed from keywords and saved in an extra table within database.
        checkList = sapply(keywords, function(x) is.matrix(x))
        if(any(checkList == TRUE)){
          # Get compensation matrix from keywords.
          listItem = which(checkList == TRUE)
          SPILLMATRIX = fcsSPILL = keywords[[listItem]]
          
          # Save colnames of compensation matrix.
          namesSPILL = colnames(fcsSPILL)
          # Creates first column with fileID.
          file_ID = rep(fcsfile, nrow(fcsSPILL))
          fcsSPILL = cbind(file_ID, fcsSPILL)
          
          # Standardizes colnames. file_ID column will be overlooked.
          cols = 1:(ncol(fcsSPILL) - 1)
          colname = NULL
          for(i in cols) {colname = c(colname, paste("col", sprintf("%02i", i), sep = ""))}
          colnames(fcsSPILL)[2:ncol(fcsSPILL)] = colname
          
          # If project_SPILLmatrix exists new rows will be added. Otherwise a new table will 
          # be generated. Same applies for the actual colnames of keywords$SPILL.
          if(dbExistsTable(DB, paste(values$projectname, "_SPILLmatrix", sep = ""))) {
            # Add actual colnames to project_SPILLcolnames.
            namesSPILL = data.frame(file_ID = rep(fcsfile, length(colname)), 
                                    SPILLmatrix_colnames = colname, namesSPILL)
            dbWriteTable(DB, paste(values$projectname, "_SPILLcolnames", sep = ""), 
                         namesSPILL, append = TRUE)
            
            # Load project_SPILLmatrix from database.
            dbSPILL = dbReadTable(DB, paste(values$projectname, "_SPILLmatrix", sep = ""))
            # Add rows of new SPILL to existing table. Samrtbind allows binding even if 
            # column number differs. Column without values will be filled with NA.
            dbSPILL = smartbind(dbSPILL, fcsSPILL)
            rownames(dbSPILL) = 1:dim(dbSPILL)[1]
            # Overwrites exsisiting SPILL table.
            dbWriteTable(DB, paste(values$projectname, "_SPILLmatrix", sep = ""), dbSPILL, 
                         overwrite = TRUE)
          }else{
            # Create project_SPILLcolnames which includes actual colnames of compensation 
            # matrix.
            namesSPILL = data.frame(file_ID = rep(fcsfile, length(colname)), 
                                    SPILLmatrix_colnames = colname, 
                                    namesSPILL)
            dbWriteTable(DB, paste(values$projectname, "_SPILLcolnames", sep = ""), namesSPILL)
            # Add SPILL table to database.
            dbWriteTable(DB, paste(values$projectname, "_SPILLmatrix", sep = ""), 
                         as.data.frame(fcsSPILL))
          }
          # Swaps compensation matrix of SPILL to a single character string.
          names(keywords)[listItem] <- "Compensation_Matrix"
          
          if(input$compensate == 1){
            keywords[[listItem]] <- paste0("File compensated and compensation matrix moved to ", values$projectname, 
                                           "_SPILLmatrix")
            # Compensate file using compensation matrix.
            fcs = exprs(compensate(fcs, SPILLMATRIX))
            
            values$SPILL <- "Yes"
            
            # Sorts columns of file according to order in values$markerNames
            fcs = fcs[, match(str_replace_all(values$markerNames[, 1], "[^[:alnum:]]", ""),
                              str_replace_all(colnames(fcs), "[^[:alnum:]]", ""))]
            #### Generate empty first column and fill it with  file ID.
            file_ID = rep(fcsfile, nrow(fcs))
            fcs = cbind(file_ID, as.data.frame(fcs))
          }else{
            keywords[[listItem]] <- paste0("Compensation matrix moved to ", values$projectname, "_SPILLmatrix")
            
            values$SPILL <- "Matrix"
            
            # Sorts columns of file according to order in values$markerNames
            fcs = fcs[, match(str_replace_all(values$markerNames[, 1], "[^[:alnum:]]", ""),
                              str_replace_all(colnames(fcs), "[^[:alnum:]]", ""))]
            #### Generate empty first column and fill it with  file ID.
            file_ID = rep(fcsfile, nrow(fcs))
            fcs = cbind(file_ID, as.data.frame(exprs(fcs)))
          }
        }else{
          
          values$SPILL <- "No"
          
          # If no matrix is available a new keyword is added to say so. Needed later on for viewing project_SPILLmatrix.
          keywords$Compensation_Matrix <- "No compensation matrix available"
          
          # Sorts columns of file according to order in values$markerNames
          fcs = fcs[, match(str_replace_all(values$markerNames[, 1], "[^[:alnum:]]", ""),
                            str_replace_all(colnames(fcs), "[^[:alnum:]]", ""))]
          
          #### Generate empty first column and fill it with  file ID.
          file_ID = rep(fcsfile, nrow(fcs))
          fcs = cbind(file_ID, as.data.frame(exprs(fcs)))
        }
        
        #### Include keywords of fcs file.
        keywords = unlist(keywords)
        # Change names of keywords.
        names(keywords) = gsub("^\\$","", names(keywords))
        # Compiles dataframe with current file_ID and keywords. Convert factors to values
        # within table.
        keyTable = data.frame(file_ID = rep(fcsfile, length(keywords)), 
                              Keywords = names(keywords), 
                              Values = unname(keywords))
        keyTable[] = lapply(keyTable, as.character)
        # keyTable is included in projectname_equipmentInfo table.
        dbWriteTable(conn = DB, paste(values$projectname, "_equipmentInfo", sep = ""), 
                     keyTable, append = TRUE)
        
        ### Include date of computation in fileIdentity table. NA is replaced by date of 
        ### current fcs file.
        sql.query= paste("update ", values$projectname, "_fileIdentity set Date_of_Measurement = '", 
                         keyTable[grep("^DATE", keyTable[, "Keywords"]), "Values"][1], 
                         "' where file_ID = ", fcsfile, sep = "")
        dbGetQuery(DB, sql.query)
        
        #### Include fcs file in table of database.
        if(dbExistsTable(conn = DB, values$projectname)) {
          sql.query = paste("select * from", values$projectname, "limit 1")
          colNum = ncol(dbGetQuery(DB, sql.query))
          
          if(ncol(fcs) == colNum){
            # FIXED FORMER COLUMN NAME MISMATCH ERROR CAUSED BY WRONG 
            # COLNAMES (FILE COLNAMES INSTEAD OF DATABASE COLNAMES):
            colnames(fcs)=c("file_ID",paste0("col", sprintf("%02i", 1:(ncol(fcs)-1))))
            dbWriteTable(conn = DB, values$projectname, fcs, append = TRUE)
          } else {
            if(ncol(fcs) > colNum) {
              # If new file has more columns than the table to be added, input$project-table 
              # will be extended until difference is settled.
              
              # Calculation of row number of input$project-table.
              sql.rowNum = paste("select max(_rowid_) from", values$projectname, "limit 1", 
                                 sep = " ")
              rowNum = dbGetQuery(DB, sql.rowNum)
              
              # Variable filled with NA accordingly to nrow() of input$project-table.
              fill = rep(NA, rowNum[1, 1])
              # For-loop creates matrix with as many columns as the difference between ncol()
              # of input$project-table and new file. At the same time the colnames for these 
              # are compiled.
              colLabeling = colNum - 1
              for(k in 1:(ncol(fcs) - colNum )) {
                # Compiles and assings standardized colname as variable name to new column.
                colname = colLabeling + k
                assign(paste("col", sprintf("%02i", colname), sep = ""), fill)
                
                # Query to add new col to input$project-table. Assign() created "colXX" name 
                # which is taken up by paste within paste-function.
                sql.queryAlter = paste("alter table", values$projectname, "add", 
                                       paste("col", sprintf("%02i", colname), sep = ""))
                dbGetQuery(DB, sql.queryAlter)
              }
              dbWriteTable(conn = DB, values$projectname, fcs, append = TRUE)
            } else {
              # If new file has less columns than input$project-table:
              # Variable filled with NA accordingly to nrow() of new file.
              fill = rep(NA, nrow(fcs))
              # For-loop creates matrix with as many columns as the difference between ncol()
              # of input$project-table and new file.
              addCol = NULL
              for(i in 1:(colNum - ncol(fcs))) {
                addCol = cbind(addCol, fill)
              }
              
              # Matrix is added to new file -> ncol(input$project-table) == ncol(fcs).
              fcs = cbind(fcs, addCol)
              dbWriteTable(conn = DB, values$projectname, fcs, append = TRUE)
            }
          }
        } else {
          # If input$project-table does not exist:
          # Generates standardized colnames accordingly to rowNum of values$markerNames of 
          # default table with file colnames and shortnames..
          changeColnames = NULL
          for (l in 1:nrow(values$markerNames)) {
            changeColnames = c(changeColnames, paste("col", sprintf("%02i", l), sep = ""))
          }
          # Changes colnames of current file. (Only happens once -> first time the table 
          # is created within database. During the following runs it only gets extended.)
          colnames(fcs) = c("file_ID", changeColnames)
          dbWriteTable(conn = DB, values$projectname, fcs)
        }
      }
      
      # After complete insertion the 'file_ID'-column is indexed. This quickens the access 
      # for later use but it takes a while here.
      sql.query = paste("create index ", values$projectname, "_idxfile on ", values$projectname, 
                        " (file_ID)", sep = "")
      dbGetQuery(DB, sql.query)
    })
  }
  
  observeEvent(input$updateProject, ignoreNULL = TRUE, {
    # Disables 'all buttons to prevent interruptions of upload.
    disable("updateProject")
    disable("restartButton")
    disable("dbSelection")
    disable("choiceProject")
    disable("classificationButton")
    disable("files")
    disable("commentFiles")
    disable("stainInfo")
    disable("decision")
    disable("compensate")
    
    output$summaryFileIdentity <- renderText({return()})
    output$summaryFiles <- renderText({paste("Those files were uploaded to the database:", 
                                             sep = "")})
    
    # Function to uplpoad data.
    includeFiles()
    
    # Shows summary of database after update.
    output$summaryProject <- renderText({
      sql.rowNum = paste("select max(_rowid_) from", values$projectname, "limit 1", sep = " ")
      rowNum = dbGetQuery(DB, sql.rowNum)
      
      sql.query = paste("select * from", values$projectname, "limit 1")
      colNum = ncol(dbGetQuery(DB, sql.query))
      
      if(values$SPILL == "No"){
        paste("You uploaded", length(values$file.paths), "file(s) to the database. Now it 
              features", rowNum, "rows and", colNum, "columns where the first column displays 
              the file ID. No compensation was performed (no available compensation matrix).", 
              sep = " ")
      }else{
        if(values$SPILL == "Yes"){
          paste("You uploaded", length(values$file.paths), "file(s) to the database. Now it 
                features", rowNum, "rows and", colNum, "columns where the first column 
                displays the file ID. The uploaded data was compensated using the compensation 
                matrix of the respective fcs header. The matrix was moved to",
                values$projectname, "_SPILLmatrix.", sep = " ")
        }else{
          paste("You uploaded", length(values$file.paths), "file(s) to the database. Now it 
                features", rowNum, "rows and", colNum, "columns where the first column 
                displays the file ID. The available compensation matrix was moved to",
                values$projectname, "_SPILLmatrix. The data was not compensated.", sep = " ")
        }
      }
      
      })
    
    # Hides close button in 'User History'-pop up.
    hide("closeModalUserHistory")
    
    # Modal window concerning user history table opens.
    toggleModal(session, "userHistory", "open")
    
    ################################################################
    # Enable everything again.
    enable("restartButton")
    enable("dbSelection")
    enable("choiceProject")
    enable("project")
    enable("classificationButton")
    enable("files")
    enable("commentFiles")
    enable("stainInfo")
    enable("compensate")
    
    # 'Staining Information' and decision on compensation are reset to their original states.
    reset("stainInfo")
    reset("compensate")
    
    # Resets 'Comment Files'.
    shinyjs::show("cancelComment")
    reset("checkboxFiles")
    reset("fileComments")
    output$fileCommentsTable <- renderDataTable({return()})
    
    # Resets everything within 'Marker Table'-pop up.
    output$diffFilesText <- renderText({return()})
    output$oddFilesText <- renderText({return()})
    output$markerNamesTable <- ({return()})
    output$shortnameInput <- ({return()})
    output$markerTable <- ({return()})
    output$loadedShortnames <- ({return()})
    hide("updateColTable")
    hide("editColTable")})
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##### TAB: DOWNLOADING DATA #####
  ##########################
  
  disable("getfiles") # only enabled when download folder and an experiment were selected
  disable("mandown")
  values$zerotable=data.frame(ID=integer(),
                              Researcher=character(),
                              Title=character()
  )
  values$flowRepTable=account$flowRep_tables$Experiment_table_short
  
  #---------------------------------------------------------------------------------------------------  
  
  output$flow=DT::renderDataTable({
    DT::datatable(values$flowRepTable,
                  options=list(searchHighlight = TRUE,orderClasses=T))
  })
  
  #---------------------------------------------------------------------------------------------------  
  
  if(exists("com.exp",where=account)){
    output$comm=DT::renderDataTable({
      DT::datatable(account$com.exp,
                    options=list(searchHighlight = TRUE,orderClasses=T))
    })
  }
  
  #---------------------------------------------------------------------------------------------------    
  
  if(exists("pre.exp",where=account)){
    output$prem=DT::renderDataTable({
      DT::datatable(account$pre.exp,
                    options=list(searchHighlight = TRUE,orderClasses=T))
    })
  }
  
  #---------------------------------------------------------------------------------------------------
  
  observe({
    if(input$tabset1=="Community Cytobank" && account$com.log==F){
      toggleModal(session,"loginc",toggle="open")
      # Saves community account data and closes pop up.
      observeEvent(input$community,ignoreNULL=T,{
        if(input$com.usr !="" && input$com.pwd !=""){
          toggleModal(session,"loginc",toggle="close")
          account$com.log<<-T
          account$com.usr<<-input$com.usr
          account$com.pwd<<-input$com.pwd
        } else {
          showNotification(id="commfail",
                           "Please enter Username and password.",
                           duration=3,
                           closeButton=F,
                           type="warning"
          )
        }
        if(account$com.log==T && !exists("com.exp",where=account)){
          withBusyIndicatorServer("community",{  
            account$sessionc<<-logIn(account$com.url,account$com.usr,account$com.pwd)
            account$com.exp<<-get.experiments(account$com.url,account$com.usr,account$com.pwd)
          })
          output$comm=DT::renderDataTable({
            DT::datatable(account$com.exp,options=list(searchHighlight = TRUE,orderClasses=T))
          })
        }
      })
    }
    
    if(input$tabset1=="Premium Cytobank" && account$pre.log==F){
      toggleModal(session,"loginp",toggle="open")
      # Saves premium account data and closes pop up.
      observeEvent(input$premium,ignoreNULL=T,{
        if(input$pre.usr !="" && input$pre.pwd !=""){
          toggleModal(session,"loginp",toggle="close")
          account$pre.log<<-T
          account$pre.usr<<-input$pre.usr
          account$pre.pwd<<-input$pre.pwd
        } else {
          showNotification(id="premfail",
                           "Please enter Username and password.",
                           duration=3,
                           closeButton=F,
                           type="warning"
          )
        }
        if(account$pre.log==T && !exists("pre.exp",where=account)){
          withBusyIndicatorServer("premium",{  
            account$sessionp<<-logIn(account$pre.url,account$pre.usr,account$pre.pwd)
            account$pre.exp<<-get.experiments(account$pre.url,account$pre.usr,account$pre.pwd)
          })
          output$prem=DT::renderDataTable({
            DT::datatable(account$pre.exp,options=list(searchHighlight = TRUE,orderClasses=T))
          })
        }
      })
    }
  })
  
  #--------------------------------------------------------------------------------------------------- 
  
  observe({
    account$flowidx<<-as.numeric(input$flow_rows_selected)
    account$commidx<<-as.numeric(input$comm_rows_selected)
    account$premidx<<-as.numeric(input$prem_rows_selected)
    
    if(input$tabset1=="FlowRepository" && length(account$flowidx)>0 ||
       input$tabset1=="Community Cytobank" && length(account$commidx)>0 ||
       input$tabset1=="Premium Cytobank" && length(account$premidx)>0
    ){
      enable("getfiles")
    } else {
      disable("getfiles")
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$getfiles,ignoreNULL=T,{
    if(input$tabset1=="FlowRepository"){
      # print(account$flowRep_tables$Experiment_table_short[account$flowidx,"ID"])
      flowproxy=DT::dataTableProxy("flow")
      path=jchoose.dir(FCS.download.path,"Select directory")
      if(length(path) != 0){
        withProgress(message="Downloading",value=0,{
          withBusyIndicatorServer("getfiles",{
            needfulFuns::getFlowRep(as.vector(
              account$flowRep_tables$Experiment_table_short[account$flowidx,"ID"]),
              path,T
            )
          })
        })
      }
      # reload table to clear previous selection
      DT::reloadData(flowproxy,resetPaging = F,clearSelection="row")
    }
    if(input$tabset1=="Community Cytobank"){
      # print(account$com.exp[account$commidx,])
      comproxy=DT::dataTableProxy("comm")
      path=jchoose.dir(FCS.download.path,"Select directory")
      if(length(path) != 0){
        withProgress(message="",value=0,{
          withBusyIndicatorServer("getfiles",{
            cytobank::project.download(account$com.url,
                                       account$com.usr,
                                       account$com.pwd,
                                       account$com.exp[account$commidx,],
                                       path)
          })
        })
      }
      DT::reloadData(comproxy,resetPaging = F,clearSelection="row")
    }
    if(input$tabset1=="Premium Cytobank"){
      # print(account$pre.exp[account$premidx,])
      premproxy=DT::dataTableProxy("prem")
      path=jchoose.dir(FCS.download.path,"Select directory")
      if(length(path) != 0){
        withProgress(message="",value=0,{
          withBusyIndicatorServer("getfiles",{
            cytobank::project.download(account$pre.url,
                                       account$pre.usr,
                                       account$pre.pwd,
                                       account$pre.exp[account$premidx,],
                                       path)
          })
        })
      }
      DT::reloadData(premproxy,resetPaging = F,clearSelection="row")
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$updateflow,ignoreNULL=T,{
    withBusyIndicatorServer("updateflow",{
      actualised=needfulFuns::updateFlowRep(FlowRepDb.path,verbose=T)
      if(actualised==T){
        showNotification(id="upflowtrue",
                         "Updating experiments",
                         duration=10,
                         closeButton=F,
                         type="message"
        )
        account$flowRep_tables<<-needfulFuns::get.tables(FlowRepDb.path)
        values$flowRepTable=account$flowRep_tables$Experiment_table_short
        output$flow=DT::renderDataTable({
          DT::datatable(values$flowRepTable,
                        options=list(searchHighlight = TRUE,orderClasses=T))
        })
      } else {
        showNotification(id="upflowfalse",
                         "Experiments are up to date",
                         duration=10,
                         closeButton=F,
                         type="message"
        )
      }
    })
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$manualprem,ignoreNULL=T,{
    output$nulltable=DT::renderDataTable({
      DT::datatable(values$zerotable,options=list(orderClasses=F,dom="t"))
    })
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$addrow,ignoreNULL=T,{
    if(input$ID != "" && input$researcher != "" && input$title != ""){
      newRow=data.frame(ID=input$ID,
                        Researcher=input$researcher,
                        Title=input$title)
      values$zerotable=rbind(values$zerotable,newRow)
      updateTextInput(session,"ID", value="")
      updateTextInput(session,"researcher", value="")
      updateTextInput(session,"title", value="")
    } else {
      showNotification(id="manfail",
                       "Fill out all text fields",
                       duration=6,
                       closeButton=F,
                       type="error"
      )
    }
  })
  
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$Del_row_head,ignoreNULL=T,{
    if(length(as.numeric(input$nulltable_rows_selected)>0)){
      row_to_del=as.numeric(input$nulltable_rows_selected)
      values$zerotable=values$zerotable[-row_to_del,]
    }
    if(nrow(values$zerotable) > 0){
      enable("mandown")
    } else {
      disable("mandown")
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observe({
    if(nrow(values$zerotable) > 0){
      enable("mandown")
    } else {
      disable("mandown")
    }
  })
  
  #---------------------------------------------------------------------------------------------------      
  
  observeEvent(input$mandown,ignoreNULL=T,{
    if(nrow(values$zerotable)>0){
      path=jchoose.dir(FCS.download.path,"Select directory")
      if(length(path) != 0){
        withProgress(message="",value=0,{
          withBusyIndicatorServer("mandown",{
            cytobank::project.download(account$pre.url,
                                       account$pre.usr,
                                       account$pre.pwd,
                                       values$zerotable,
                                       path)
          })
        })
      }
      values$zerotable=data.frame(ID=integer(),
                                  Researcher=character(),
                                  Title=character()
      )
      disable("mandown")
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$FileInfo,ignoreNULL=T,{
    if(length(account$flowidx) != 1){
      showNotification(id="infofail",
                       "Select one experiment to show informations",
                       duration=6,
                       closeButton=F,
                       type="error"
      )
    } else {
      if(values$flowRepTable[account$flowidx,"PMID"] != "not applicable"){
        proxy=DT::dataTableProxy("flow")
        infoID=as.numeric(values$flowRepTable[account$flowidx,"PMID"])
        infoObject=EUtilsGet(infoID)
        toggleModal(session, "pub", toggle = "open")
        at=paste(strong("Article Title:"),br(),ArticleTitle(infoObject))
        jo=paste(strong("Journal:"),br(),MedlineTA(infoObject))
        vo=paste(strong("Volume:"),br(),Volume(infoObject))
        is=paste(strong("Issue:"),br(),Issue(infoObject))
        pa=paste(strong("Pages:"),br(),MedlinePgn(infoObject))
        ab=paste(strong("Abstract:"),br(),AbstractText(infoObject))
        output$filetext=renderUI({
          HTML(paste(at,
                     jo,
                     vo,
                     is,
                     pa,
                     ab,
                     sep='<br/><br/>'
          )
          )
        })
        DT::reloadData(proxy,resetPaging=F,clearSelection="row")
      } else {
        showNotification(id="appfail",
                         "No information available",
                         duration=6,
                         closeButton=F,
                         type="error"
        )
      }
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$enterPMID,ignoreNULL=T,{
    if(length(account$flowidx)>0){
      toggleModal(session,"addPMID","open")
    } else {
      showNotification(id="pmidfail",
                       "Select one or more experiments to enter PMID",
                       duration=6,
                       closeButton=F,
                       type="error"
      )
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$savePMID,ignoreNULL=T,{
    if(!is.na(as.numeric(input$PMID)) && input$PMID!=""){
      values$flowRepTable[account$flowidx,"PMID"]=input$PMID
      withBusyIndicatorServer("savePMID",{
        needfulFuns::writeDB(FlowRepDb.path,
                             "Experiment_table_short",values$flowRepTable,replace=T,verbose=T)
        updateTextInput(session,"PMID", value="")
      })
      account$flowRep_tables<<-needfulFuns::get.tables(FlowRepDb.path)
      values$flowRepTable=account$flowRep_tables$Experiment_table_short
      # output$flow=DT::renderDataTable({
      #   DT::datatable(values$flowRepTable,
      #                 options=list(searchHighlight = TRUE,orderClasses=T))
      # })
      proxy=DT::dataTableProxy("flow")
      DT::reloadData(proxy,resetPaging=F,clearSelection="row")
      toggleModal(session,"addPMID","close")
    } else {
      updateTextInput(session,"PMID", value="")
      showNotification(id="numericfail",
                       "PubMed ID(s) have to be numeric",
                       duration=6,
                       closeButton=F,
                       type="error"
      )
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$discard,ignoreNULL=T,{
    proxy=DT::dataTableProxy("flow")
    updateTextInput(session,"PMID", value="")
    toggleModal(session,"addPMID","close")
    DT::reloadData(proxy,resetPaging=F,clearSelection="row")
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$enterkey,ignoreNULL=T,{
    if(length(account$flowidx)>0){
      toggleModal(session,"addkey","open")
    } else {
      showNotification(id="keyfail",
                       "Select one or more experiments to enter keywords",
                       duration=6,
                       closeButton=F,
                       type="error"
      )
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$savekey,ignoreNULL=T,{
    if(nchar(input$keyword)>0){
      for(i in account$flowidx){
        if(nchar(values$flowRepTable[i,"Keywords"])>0){
          values$flowRepTable[i,"Keywords"]=paste(c(values$flowRepTable[i,"Keywords"],
                                                               input$keyword),collapse = ", ")
        } else {
          values$flowRepTable[i,"Keywords"]=input$keyword
        }
      }
      withBusyIndicatorServer("savekey",{
        needfulFuns::writeDB(FlowRepDb.path,"Experiment_table_short",
                             values$flowRepTable,replace=T,verbose=T)
        updateTextInput(session,"keyword", value="")
      })
      account$flowRep_tables<<-needfulFuns::get.tables(FlowRepDb.path)
      values$flowRepTable=account$flowRep_tables$Experiment_table_short
      # output$flow=DT::renderDataTable({
      #   DT::datatable(values$flowRepTable,
      #                 options=list(searchHighlight = TRUE,orderClasses=T))
      # })
      proxy=DT::dataTableProxy("flow")
      DT::reloadData(proxy,resetPaging=F,clearSelection="row")
      toggleModal(session,"addkey","close")
    } else {
      updateTextInput(session,"keyword", value="")
      showNotification(id="keytextfail",
                       "Enter one or more keywords",
                       duration=6,
                       closeButton=F,
                       type="error"
      )
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$discardkey,ignoreNULL=T,{
    proxy=DT::dataTableProxy("flow")
    updateTextInput(session,"keyword", value="")
    toggleModal(session,"addkey","close")
    DT::reloadData(proxy,resetPaging=F,clearSelection="row")
  })
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##### TAB: PLOTTING DATA #####
  ##########################
  #### Tabpanel: Search ####
  ### Search multiple databases.
  # Select databases.
  output$dbMultiSearch <- renderUI({
    #### PATH TO DATABASE SELECTION ####
    selectizeInput("searchMultiDB", "Choose a Database:", multiple = TRUE,
                   choices = sub(".sqlite3", "", list.files(DB.path, 
                                                            pattern = "sqlite3$")),
                   options = list(placeholder = "", 
                                  onInitialize = I('function() {this.setValue(""); }')))
  })
  
  observeEvent(input$searchMultiDB, {
    updateFields()
  })
  
  updateFields <- eventReactive(input$searchMultiDB, {
    output$multiDBprojects <- renderUI({return()})
    output$availableMultiMarker <- renderUI({return()})
    shinyjs::hide("createMultiSearch")
  })
  
  # If 'Done' clicked, information of all projects from each selected databases is extracted.
  filterInfo <- eventReactive(input$done, ignoreNULL = TRUE,{
    projectInfo = list()
    for(db in 1:length(input$searchMultiDB)){
      # Connect to first database in list.
      tempDB <<- dbConnect(SQLite(), dbname = paste(DB.path, 
                                                    input$searchMultiDB[db], ".sqlite3", sep = ""))
      # Get all projects within DB.
      projects = DBprojects(tempDB)
      
      # Gets all projects within database.
      tables = dbListTables(tempDB)
      # Get all identification table names.
      tables = tables[grep("Identity$", tables)]
      # Get content of all tables.
      fileIdentity = lapply(tables[grep("_fileIdentity", tables)], function(x) dbReadTable(tempDB, x))
      markerIdentity = lapply(tables[grep("_markerIdentity", tables)], function(x) dbReadTable(tempDB, x))
      # Change list names to project names.
      names(fileIdentity) = tables[grep("_fileIdentity", tables)]
      names(markerIdentity) = tables[grep("_markerIdentity", tables)]
      # Get project name.
      if(all(sub("_fileIdentity", "", tables[grep("_fileIdentity", tables)]) == 
             sub("_markerIdentity", "", tables[grep("_markerIdentity", tables)]))){
        # Check if tables are empty.
        if(all(unlist(lapply(names(fileIdentity), function(x) nrow(fileIdentity[[x]]) > 0))) &
           all(unlist(lapply(names(fileIdentity), function(x) nrow(fileIdentity[[x]]) > 0)))){
          # Add identification tables to list.
          projectInfo[[input$searchMultiDB[db]]] = c(fileIdentity, markerIdentity)
        }
      }
      # Sever connection to database.
      dbDisconnect(tempDB) 
    }
    values$projectInfo = projectInfo
  })
  
  # Render "Select multiple projects'.
  observeEvent(input$done, ignoreNULL = TRUE, {
    filterInfo()
    output$multiDBprojects <- renderUI({
      # If projectInfo list was generated, all projects within will be listedfor selection.
      validate(need(length(values$projectInfo) > 0, "Some database projects seem to be empty. 
                    Please revise your database selection."))
      # Get project names.
      projectnames = unlist(lapply(1:length(values$projectInfo), 
                                   function(x) unique(names(values$projectInfo[[x]]))))
      values$projectnames = unique(sub("_[^_]+$", "", projectnames))
      # List all projects
      selectInput(inputId = "dbProjects", "Select projects of interest", choices = values$projectnames, 
                  multiple = TRUE, selected = NULL)
    })
  })
  
  # Returns all marker within all projects.
  observeEvent(input$dbProjects, ignoreNULL = TRUE, {
    # Lists all markers within one project.
    output$availableMultiMarker <- renderUI({
      selectInput("searchMultiMarker", "Select markers of interest", choices = searchMultiNames(), 
                  multiple = TRUE, selected = NULL)
    })
  })
  
  observeEvent(input$searchMultiMarker, ignoreNULL = TRUE, {
    if(!is.null(input$searchMultiMarker)){
      shinyjs::show("createMultiSearch")
    }
  })
  
  # Extracts marker names depending on project choice.
  searchMultiShortnames <- reactive({
    # Get all unique markers from project list.
    unique(unlist(lapply(1:length(values$projectInfo), function(x){
      # Get marker identification table position in list.
      name = grep("_markerIdentity", names(values$projectInfo[[x]]))
      
      unlist(lapply(input$dbProjects, function(y){
        if(paste0(y, "_markerIdentity") %in% names(values$projectInfo[[x]])){
          id = grep(paste0(y, "_markerIdentity"), names(values$projectInfo[[x]]))
          # Get marker from table.
          unique(values$projectInfo[[x]][[id]][,"shortname"])
        }
      }))
    })))
  })
  
  # Uses searchShortname() and removes variables which aren't of interest.
  searchMultiNames <- eventReactive(input$dbProjects, {
    names = searchMultiShortnames()
    # Searches for any matches.
    present = toupper(names)
    present = grep(paste(c("TIME", "EVENT"), collapse = "|"), present)
    
    # If variable 'present' != 0 the content will be removed from names displayed. 
    if(length(present) == 0){names}else{names[-present]}
  })
  
  # Retrieves all relevant data from/for search.
  inputMultiSearch <- eventReactive(input$createMultiSearch, {
    # Retrieves data from projects.
    # If more than one marker was selected, only files with all selected markers are of interest.
    # Therefore only fileIDs that occur as often as count of marker selection are saved.
    final.table = data.frame()
    if(length(input$searchMultiMarker) >= 1){    
      # Go through all selected marker identification tables.
      
      #for(i in 1:length(input$dbProjects)){
      for(x in 1:length(values$projectInfo)){
        if(any(input$dbProjects %in% unique(sub("_[^_]+$", "", names(values$projectInfo[[x]]))))){
          # Go through all selected projects.
          for(i in 1:length(input$dbProjects)){
            # If project i is in database x the following code is executed.
            if(input$dbProjects[i] %in% unique(sub("_[^_]+$", "", names(values$projectInfo[[x]])))){
              
              # Get marker and file identification table position in list.
              markertableID = grep(paste0(input$dbProjects[i], "_markerIdentity"), names(values$projectInfo[[x]]))
              fileID = grep(paste0(input$dbProjects[i], "_fileIdentity"), names(values$projectInfo[[x]]))
              
              # Compile final search table.
              for(fn in 1:length(markertableID)){
                a = markertableID[fn]
                fileIDs = values$projectInfo[[x]][[a]][which(grepl(
                  paste(trimSearch(input$searchMultiMarker), collapse = "|"), 
                  trimSearch(values$projectInfo[[x]][[a]][,"shortname"])) == TRUE), "file_ID"]
                
                fileIDs = names(which(table(fileIDs) == length(input$searchMultiMarker)))
                
                # Extract filenames of files which show all selected marker names.
                if(length(fileIDs) > 0){
                  # Get file identification table name.
                  tempFile = sub("_markerIdentity", "_fileIdentity", names(values$projectInfo[[x]][a]))
                  
                  tempFile = values$projectInfo[[x]][[eval(tempFile)]][fileIDs, "filename"]
                  
                  # Create table with all relevant Info.
                  temp.table = data.frame(Database = rep(names(values$projectInfo[x]), length(fileIDs)),
                                          Project = rep(sub("_markerIdentity", "", names(values$projectInfo[[x]][a])), 
                                                        length(fileIDs)),
                                          File_ID = as.integer(fileIDs), 
                                          Filename = tempFile)
                  # Add table to information of other projects.
                  final.table = rbind(final.table, temp.table)
                }
              }
            }
          }
        }
      }
    }
    # Note selected marker names.
    marker = data.frame(Database = paste0("Searched markers: ", paste0(input$searchMultiMarker, collapse = ", ")), 
                        Project = "", 
                        File_ID = "",
                        Filename = "")
    
    if(nrow(final.table) != 0){
      # Save complete table in reactiveValues().
      values$searchTable = rbind(final.table, marker)
    }else{values$searchTable = NULL}
  })
  
  # Displays search results in table format.
  output$searchTable <- renderTable({
    inputMultiSearch()
    # If a file is selected it will be displayed in table format -> one below the other.
    validate(need(!is.null(values$searchTable), "No files with all selected marker names exist. Revise your selection."))
    values$searchTable
  }, striped = TRUE, include.colnames = TRUE)
  
  
  # Download search table as csv.
  observeEvent(input$downloadSearch, ignoreNULL = TRUE, {
    if(!is.null(values$searchTable)){
      # Download created table.
      path = jchoose.dir("Save File To")
      if(length(path) != 0){
        withBusyIndicatorServer("downloadTable", {
          write.csv2(values$searchTable, 
                     file = paste0(path, "\\", "Marker-Search", "_", Sys.Date(), ".csv"),
                     row.names = FALSE)})
      }
    }
  })
  
  
  ########################
  #### Tabpanel: Plot ####
  # Loading of database and listing of all projects within it.
  output$plotDatabases <- renderUI({
    #### PATH TO DATABASE SELECTION ####
    selectizeInput("plotDBSelection", "Choose a Database:", 
                   choices = sub(".sqlite3", "", list.files(DB.path, 
                                                            pattern = "sqlite3$")),
                   options = list(placeholder = "", 
                                  onInitialize = I('function() {this.setValue(""); }')))
  })
  
  observeEvent(input$plotDBSelection, ignoreNULL = TRUE, {
    hide("plot-content")
    shinyjs::show("hidePlot")
    output$hidePlot <- renderPlot({},  width = 450, height = 450)
    
    if(nchar(input$plotDBSelection) == 0){
      return(NULL)
    }else{
      # Otherwise a database connection is established.
      #### PATH TO LOAD DATABASE ####
      plotDB <<- dbConnect(SQLite(), dbname = paste(DB.path, 
                                                    input$plotDBSelection, ".sqlite3", sep = ""))
      
      output$selectProject <- renderUI({
        validate(need(length(dbListTables(plotDB)) > 0, "Database is empty. No projects exist."))
        selectInput(inputId = "plotProject", "Select a Project", choices = DBprojects(plotDB), 
                    selected = DBprojects(plotDB)[1])
      })  
    }
  })
  
  # Lists all files within selected project.
  observeEvent(input$plotProject, ignoreNULL = TRUE, {
    # Removes drop-down list of files. (If another project was chosen beforehand the widget
    # is displayed. Now it is cleared.)
    output$selectFile <- renderUI({return()})
    
    # Only executed if a project is selected.
    if(nchar(input$plotProject) != 0){
      # 'If'-loop to prevent errors when project is empty.
      if(dbExistsTable(plotDB, paste0(input$plotProject, "_fileIdentity"))){
        if(nrow(dbReadTable(plotDB, paste0(input$plotProject, "_fileIdentity"))) != 0) {
          shinyjs::show("plot-content")
          # Drop-down list of files.
          output$selectFile <- renderUI({
            selectInput("plotFile", "Select a file", choices = fileList(), selected = fileList()[1])
          })
          
          # Gets all files in selected project.
          fileList <- reactive({
            sql.query = paste(input$plotProject, "_fileIdentity", sep="")
            values$fileTable <- dbReadTable(plotDB, sql.query)
            values$fileTable[, "filename"]
          })
          
          # Clears warning. (If an empty project was chosen beforehand that ensures that text 
          # is removed to prevent misunderstandings.)
          output$emptyProject <- renderText({return()})
        }else{
          # Returns warning if no files are within project_fileIdentity.
          output$emptyProject <- renderText({
            paste("The project includes no files. Please choose another one.")
          })
        }
      }else{
        # Returns warning if no project_fileIdentity exists.
        output$emptyProject <- renderText({
          paste("The project is corrupt. No file table and therefore no files exist. Please 
                choose another one.")
        })
        }
      }
    })
  
  # Returns three bars with possible variables for plot. User chooses which variable goes on which
  # axis. Default setting are the first three shortnames differing from 'FSC, SSC, Time, Event'.
  observeEvent(input$plotFile, ignoreNULL = TRUE, {
    # Needed for default setting. FSC, SSC, Time and Event are removed from list.
    default = plotNames()
    if(length(grep("^FSC", default, ignore.case = TRUE))!=0){
      default = default[-grep("^FSC", default, ignore.case = TRUE)]}
    if(length(grep("^SSC", default, ignore.case = TRUE))!=0){
      default = default[-grep("^SSC", default, ignore.case = TRUE)]}
    if(length(grep("Time", default, ignore.case = TRUE))!=0){
      default = default[-grep("Time", default, ignore.case = TRUE)]}
    if(length(grep("Event", default, ignore.case = TRUE))!=0){
      default = default[-grep("Event", default, ignore.case = TRUE)]}
    
    # Variable for x-axis.
    output$selectX <- renderUI({
      selectInput("varX", "Select X variable for plot", choices = plotNames(), 
                  selected = default[1])
    })
    # Variable for x-axis.
    output$selectY <- renderUI({
      selectInput("varY", "Select Y variable for plot", choices = plotNames(), 
                  selected = default[2])
    })
    # Variable for x-axis.
    output$selectLegend <- renderUI({
      selectInput("varLegend", "Select Z variable for plot", choices = plotNames(), 
                  selected = default[3])
    })
  })
  
  # Extracts column names related to chosen file.
  shortnames <- reactive({
    # Gets fileID of selected file.
    fileID = values$fileTable[grep(input$plotFile, values$fileTable[,"filename"]), "file_ID"]
    # Extracts shortnames depending on fileID.
    sql.query = paste("select shortname from ", input$plotProject, "_markerIdentity where file_ID == '", 
                      fileID,"'", sep = "")
    c(t(dbGetQuery(plotDB, sql.query)))
  })
  
  # Uses shortname() and removes variables which should not be plotted.
  plotNames <- reactive({
    names = shortnames()
    # Searches for any matches.
    present = toupper(names)
    present = grep(paste(c("TIME", "EVENT"), collapse = "|"), present)
    
    # If variable 'present' != 0 the content will be removed from names displayed for plotting purpose. 
    if(length(present) == 0){names}else{names[-present]}
  })
  
  binplot <- function(data, set.cex = 1.0, set.mgp = c(1.4, 0.5, 0), bin.size = 0.2, mincells = 10) { 
    # Margin of axis
    xmin.val = 0
    xmax.val = 14
    ymin.val = 0
    ymax.val = 14
    
    # Names of axis coming from colnames of selected variables.
    cnames = colnames(data)
    # Margin matrix bin wise
    fX = cut(data[,1], breaks = seq(xmin.val, xmax.val, by = bin.size), include.lowest = TRUE)
    fY = cut(data[,2], breaks = seq(ymin.val, ymax.val, by = bin.size), include.lowest = TRUE)
    # Matrix of margins (] -> Quantity of cell groups.
    tab = table(fX, fY)
    colnames(tab) = seq(ymin.val, ymax.val - bin.size, by = bin.size)
    rownames(tab) = seq(xmin.val, xmax.val - bin.size, by = bin.size)
    
    # Create empty plot.
    plot(1, type = 'n', frame.plot = FALSE, xlim = c(xmin.val, xmax.val+2),
         ylim = c(ymin.val-0.5, ymax.val+1.0), xlab = cnames[1], ylab = cnames[2], 
         cex.lab = 1.2*set.cex, cex.axis = 0.9*set.cex, mgp = set.mgp)
    
    # Bin grid, matrix is rewitten to one row.
    fXY = as.factor(paste(fX, fY))
    
    # Zcutoff data smaller than is zero is set to zero.
    data[which(data[,3] < 0), 3] = 0
    
    # Depending on calculation method selection (MFI, Frequency, Density) data is processed.
    if(input$calcMethod == 3){
      # Number of cells in bin
      my.calc = aggregate(data[,3], by = list(fXY), length)
      # For plot title
      calcMethod = "Density"
    }else{
      if(input$calcMethod == 1){
        my.calc = aggregate(data[,3],by = list(fXY), mean)
        # For plot title
        calcMethod = "MFI"
      }else if (input$calcMethod == 2) {
        # Calculation per bin frequence. Selects x-y-coordinate.
        my.calc = aggregate(data[,3], by = list(fXY), function(x) {
          y = round(100 * length(which(x >= input$zCutoff))/length(x))
          return(y)
        })
        # For plot title
        calcMethod = "Frequency"
      } 
    }
    
    # Number of cells in bin. Cell quantity per bin is added to table (3 columns: coordinate, 
    # frequence, ncells).
    my.lengths = aggregate(data[,3], by = list(fXY), length)
    my.calc = cbind(my.calc, ncells = my.lengths$x)
    
    # Bin color factor depending on calculation method.
    if(input$calcMethod == 2){
      my.calc.fac = cut(my.calc$x, breaks = seq(0, 100, by = 10), labels = 1:10, 
                        include.lowest = TRUE)
      # Dynamic range.
      min.legend = ymin.val
      max.legend = max.range = (ymax.val - ymin.val)/2
      # Legend steps
      step = round(diff(range(max.legend, min.legend))/10,1)
      steps = seq(min.legend,max.legend, by = step)
      label.steps = seq(0, 100, by = 10)
    }else{
      # No min max display in z dimension.
      idx = which(my.calc$ncells >= mincells)
      min.range = floor(min(my.calc[idx, 'x'])*10)/10
      max.range = ceiling(max(my.calc[idx, 'x'])*10)/10
      
      # Legend steps
      step = round(diff(range(max.range, min.range))/10, 2)
      steps = seq(min.range, max.range, by = step)
      label.steps = steps[1:11]
      
      my.calc.fac = cut(my.calc$x,breaks = steps, labels = 2:11)
    }
    
    levels(my.calc.fac) = c(0, levels(my.calc.fac), 11, 12)
    my.calc = cbind(my.calc, fac = as.numeric(my.calc.fac)+1)
    
    # Legend colors from blue to red (10 colors)
    cols = c("#0024FFFF", "#0024FFFF", "#0092FFFF", "#00FFFFFF", "#00ff80", "#4dff4d", "#B6FF00FF",
             "#FDFE02", "#FFDB00FF", "#FF6D00FF", "#FF0000FF", "#FF0000FF")
    
    bincount = 0
    maxcells = 0
    
    # Actual plotting
    for(x in rownames(tab)) {
      for(y in colnames(tab)) {
        if(tab[x,y] >= mincells) {
          # String is composed of colname and rowname so that first column can be searched.
          fact = as.factor(paste('(', x, ',', as.numeric(x) + bin.size, '] ', '(', y, ',',
                                 as.numeric(y)+bin.size, ']', sep = ""))
          # Index of fact.
          idx = which(as.character(fact) == as.character(my.calc$Group.1))
          # Bin is drawn as rectangle. X decides on factor/color.
          rect(x, y, as.numeric(x)+bin.size, as.numeric(y)+bin.size, 
               col = cols[my.calc[idx, 'fac']], border = NA)
          bincount = bincount + 1      
          if(tab[x,y] > maxcells) maxcells = tab[x,y]     
        }
      }
    }
    
    ### xcutoff, ycutoff and zcutoff per input
    # Positioning of legend
    rect.size = bin.size
    space = -rect.size/2
    
    # Add quadrant frequencies.
    ncells = dim(data)[1]
    if(input$xCutoff > 0 & input$yCutoff > 0) {
      # Add gate line to plot.
      abline(v = input$xCutoff, col = "darkgrey")
      abline(h = input$yCutoff, col = "darkgrey")
      
      # Calculate quadrant frequencies.
      q1.total = abs(100*length(which(data[,1] <= input$xCutoff &  data[,2]
                                      <= input$yCutoff))/ncells)
      q2.total = abs(100*length(which(data[,1] > input$xCutoff &  data[,2]
                                      <= input$yCutoff))/ncells)
      q3.total = abs(100*length(which(data[,1] > input$xCutoff &  data[,2]
                                      > input$yCutoff))/ncells)
      q4.total = abs(100-q1.total-q2.total-q3.total)
      # Add frequencies to plot.
      text(xmin.val+3*rect.size, ymin.val+0.3*rect.size, label = sprintf("%0.1f%%", q1.total),
           cex = 0.8*set.cex, pos = 1)
      text(xmax.val+14*rect.size, ymin.val-3*rect.size, label = sprintf("%0.1f%%", q2.total),
           cex = 0.8*set.cex, pos = 2)
      text(xmax.val+14*rect.size, ymax.val+4*rect.size, label = sprintf("%0.1f%%", q3.total),
           cex = 0.8*set.cex, pos = 2)
      text(xmin.val+3*rect.size, ymax.val+rect.size, label = sprintf("%0.1f%%", q4.total),
           cex = 0.8*set.cex, pos = 3)
    }
    
    # Get only the cells which are greater than 0 in col 1 and 2.
    data.zero = data[data[,1] >= 0 & data[,2] >= 0,]
    ncells.zero = nrow(data.zero)
    
    # Print legend and label
    min.legend = ymin.val
    max.legend = (ymax.val-ymin.val)/2
    legend.step = round(diff(range(max.legend,min.legend))/10,1)
    if (input$calcMethod == 3) { 
      label.steps = round(label.steps,-2)
      label.steps[1] = min.range
    }
    legend.steps = seq(min.legend, max.legend, by = legend.step)
    
    j = 2
    for(i in legend.steps[1:11]) {
      # Legend
      if(j<12) {
        rect(xmax.val-0.16, i, xmax.val+0.6, i+legend.step, col = cols[j], border = NA)
      }
      # Label depending on calculation method
      if(input$calcMethod == 1) {
        text(xmax.val+0.39, i, label = sprintf("%s", label.steps[j-1]), cex = 0.8*set.cex, pos = 4)
      }else {
        text(xmax.val+0.39, i, label = sprintf("%0.1f", label.steps[j-1]), cex = 0.8*set.cex, pos = 4)
      }
      j = j + 1
    }
    
    # Legend title
    text(xmax.val+2.1, legend.steps[11]+0.8, label = sprintf("%s", cnames[3]), cex = 1.0*set.cex,
         adj = 1)
    
    # Add title to plot.
    firstLine = sprintf("%s: %s, cutoff=%0.1f", input$plotFile, calcMethod, input$zCutoff)
    secondLine = sprintf("%s-%s(%0.1f%%); min/max=%s/%s; binsize=%s,#bins=%s", ncells, ncells.zero,
                         (ncells.zero/ncells*100), mincells, maxcells, bin.size, bincount)
    title(main = firstLine, line = 3.2, cex.main = 0.9, adj = 0)
    title(main = secondLine, line = 2.4, cex.main = 0.7, adj = 0)
  }
  
  # Function to update plot only if plotGraphic-button is pressed -> observes event and only
  # executes when event occurs.
  updatePlot <- eventReactive(input$plotGraphic, {
    disable("plot-content")
    disable("plotDatabases")
    disable("selectProject")
    
    withBusyIndicatorServer("plotGraphic", {
      # Checks if all cutoffs differ from NULL and 0. If not warning is shown and nothing further
      # happens.
      if(input$calcMethod == 2 & input$zCutoff == 0) {
        toggleModal(session, "warningCutoff", "open")
        output$missingCutoffs <- renderText({
          paste("Please provide a cutoff larger than 0 for z variable.")
        })
      }else{
        # Command to display plot (based on updatePlot()-function).
        # Spinning circle occurs and disappears as soon as calculation is finished.
        fileID = values$fileTable[grep(input$plotFile, values$fileTable[,"filename"]), "file_ID"]
        
        sql.query = paste("select * from ", input$plotProject, "where file_ID == ", fileID)
        # Loading values from project table.
        fcs = dbGetQuery(plotDB, sql.query)[, -1]
        # Replace standardized colnames with actual ones from project_markerIdentity.
        colnames(fcs) = shortnames()
        # Cutting file to reduce computing time.
        fcs = fcs[, c(input$varX, input$varY, input$varLegend)]
        
        # Computes the hyperbolic arcsine of numeric data. Needed for reasonable display of data.
        fcs = asinh(fcs)
        binplot(fcs)
      }
      hide("hidePlot")
      enable("plot-content")
      enable("plotDatabases")
      enable("selectProject")
    })
  })
  
  output$plot <- renderPlot({
    updatePlot()
  }, width = 450, height = 450)
  
  observeEvent(input$closeCutoff, ignoreNULL = TRUE, {
    toggleModal(session, "warningCutoff", "close")
  })
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##### TAB: REVIEWING TABLES ##### 
  # Shows table example to explain layout.
  output$tableLayout <- renderImage({
    return(list(
      src = paste0(App.path, "www/Images/tableLayoutPic.png"),
      filetype = "image/png"
    ))
  }, deleteFile = FALSE)
  
  ##### DOWNLOAD TABLES #####
  observeEvent(input$downloadTable, ignoreNULL = TRUE, {
    if(input$downloadChoice == 1){
      # Download file identification.
      validate(need(nchar(input$fileIdentityTable) > 0, "Select a table to download."))
      path = jchoose.dir("Save File To")
      if(length(path) != 0){
        withBusyIndicatorServer("downloadTable", {
          write.csv2(dbReadTable(tableDB, input$fileIdentityTable)[, -5], 
                     file = paste0(path, "\\", as.character(input$reviewDB), "-", 
                                   input$fileIdentityTable, "_", Sys.Date(), ".csv"),
                     row.names = FALSE)})
      }
    }else{
      if(input$downloadChoice == 2){
        # Download experiment classification.
        validate(need(nchar(input$fileCommentTable) > 0, "Select a table to download."))
        path = jchoose.dir("Save File To")
        if(length(path) != 0){
          withBusyIndicatorServer("downloadTable", {
            write.csv2(dbReadTable(tableDB, input$fileCommentTable), 
                       file = paste0(path, "\\", as.character(input$reviewDB), "-",
                                     input$fileCommentTable, "_", Sys.Date(), ".csv"),
                       row.names = FALSE)})
        }
      }else{
        if(input$downloadChoice == 3){
          # Download marker identification.
          validate(need(nchar(input$markerIdentityTable) > 0, "Select a table to download."))
          path = jchoose.dir("Save File To")
          if(length(path) != 0){
            withBusyIndicatorServer("downloadTable", {
              write.csv2(dbReadTable(tableDB, input$markerIdentityTable), 
                         file = paste0(path, "\\", as.character(input$reviewDB), "-", 
                                       input$markerIdentityTable, "_", Sys.Date(), ".csv"),
                         row.names = FALSE)})
          }
        }else{
          if(input$downloadChoice == 4){
            # Download experiment classification.
            validate(need(nchar(input$classificationTable) > 0, "Select a table to download."))
            path = jchoose.dir("Save File To")
            if(length(path) != 0){
              withBusyIndicatorServer("downloadTable", {
                write.csv2(dbReadTable(tableDB, input$classificationTable), 
                           file = paste0(path, "\\", as.character(input$reviewDB), "-",
                                         input$classificationTable, "_", Sys.Date(), ".csv"),
                           row.names = FALSE)})
            }
          }else{
            if(input$downloadChoice == 5){
              # Download equipment information.
              validate(need(nchar(input$equipmentInfoTable) > 0, "Select a table to download."))
              path = jchoose.dir("Save File To")
              if(length(path) != 0){
                withBusyIndicatorServer("downloadTable", {
                  write.csv2(dbReadTable(tableDB, input$equipmentInfoTable), 
                             file = paste0(path, "\\", as.character(input$reviewDB), "-", 
                                           input$equipmentInfoTable, "_", Sys.Date(), ".csv"),
                             row.names = FALSE)})
              }
            }else{
              # Download user history.
              path = jchoose.dir("Save File To")
              if(length(path) != 0){
                withBusyIndicatorServer("downloadTable", {
                  write.csv2(dbReadTable(tableDB, "UserHistory"), 
                             file = paste0(path, "\\", as.character(input$reviewDB), "-UserHistory_", 
                                           Sys.Date(), ".csv"), row.names = FALSE)})
              }
            }
          }
        }
      }
    }
  })
  
  ##### VIEW FILEINDENTITY, MARKERIDENTITY AND CLASSIFICATION TABLE #####
  # User loads database to view identity tables within.
  observeEvent(input$reviewDB, ignoreNULL = TRUE, {
    output$fileTable <- renderDataTable({return()})
    output$commentTable <- renderDataTable({return()})
    output$markerTable <- renderDataTable({return()})
    output$classTable <- renderDataTable({return()})
    output$equipTable <- renderDataTable({return()})
    
    # If no database was selected nothing happens.
    if(nchar(input$reviewDB) == 0){
      return(NULL)
    }else{
      # Otherwise a database connection is established.
      #### PATH TO LOAD DATABASE ####
      tableDB <<- dbConnect(SQLite(), dbname = paste(DB.path, 
                                                     input$reviewDB, ".sqlite3", sep = ""))
      
      ### TabPanel: File Identification
      # Function fetches all fileIdentity tables within database.
      selectFileTable <- reactive({
        tables = dbListTables(tableDB)
        tables[grep("fileIdentity$", tables)]
      })
      
      # Shows user all fileIdentity tables within database.
      output$fileTableChoice <- renderUI({
        validate(need(length(dbListTables(tableDB)) > 0, "Database is empty. No projects exist."))
        validate(need(any(grepl("_fileIdentity",dbListTables(tableDB))), 
                      "Database corrupt. No files within it."))
        
        selectizeInput("fileIdentityTable", "Choose File Identification Table", 
                       choices = selectFileTable(),
                       options = list(placeholder = "", 
                                      onInitialize = I('function() {this.setValue(""); }')))
      })
      
      
      ### TabPanel: File Commentary
      # Function fetches all classification tables within database.
      selectCommentTable <- reactive({
        tables = dbListTables(tableDB)
        tables[grep("fileComments$", tables)]
      })
      
      # Shows user all fileComments tables within database.
      output$commentTableChoice <- renderUI({
        validate(need(length(dbListTables(tableDB)) > 0, "Database is empty. No projects exist."))
        validate(need(any(grepl("_fileComments",dbListTables(tableDB))), 
                      "No files were commented."))
        
        selectizeInput("fileCommentTable", "Choose File Commentary table", 
                       choices = selectCommentTable(),
                       options = list(placeholder = "", 
                                      onInitialize = I('function() {this.setValue(""); }')))
      })
      
      
      ### TabPanel: Marker Identification
      # Function fetches all markerIdentity tables within database.
      selectMarkerTable <- reactive({
        tables = dbListTables(tableDB)
        tables[grep("markerIdentity$", tables)]
      })
      
      # Shows user all markerIdentity tables within database.
      output$markerTableChoice <- renderUI({
        validate(need(length(dbListTables(tableDB)) > 0, "Database is empty. No projects exist."))
        validate(need(any(grepl("_markerIdentity",dbListTables(tableDB))), 
                      "Database corrupt. No files within it."))
        
        selectizeInput("markerIdentityTable", "Choose Marker Table", 
                       choices = selectMarkerTable(),
                       options = list(placeholder = "", 
                                      onInitialize = I('function() {this.setValue(""); }')))
      })
      
      
      ### TabPanel: Experiment Classification
      # Function fetches all classification tables within database.
      selectClassTable <- reactive({
        tables = dbListTables(tableDB)
        tables[grep("Classification$", tables)]
      })
      
      # Shows user all Classification tables within database.
      output$classTableChoice <- renderUI({
        # output$classTable <- renderDataTable({return()})
        validate(need(length(dbListTables(tableDB)) > 0, "Database is empty. No projects exist."))
        validate(need(any(grepl("_Classification",dbListTables(tableDB))), 
                      "No project information provided."))
        
        selectizeInput("classificationTable", "Choose classification table", 
                       choices = selectClassTable(),
                       options = list(placeholder = "", 
                                      onInitialize = I('function() {this.setValue(""); }')))
      })
      
      
      ### TabPanel: Equipment Information
      # Function fetches all fileIdentity tables within database.
      selectEquipTable <- reactive({
        tables = dbListTables(tableDB)
        tables[grep("equipmentInfo$", tables)]
      })
      
      # Shows user all fileIdentity tables within database.
      output$equipTableChoice <- renderUI({
        validate(need(length(dbListTables(tableDB)) > 0, "Database is empty. No projects exist."))
        validate(need(any(grepl("_equipmentInfo",dbListTables(tableDB))), 
                      "Database corrupt. Table does not exist."))
        
        selectizeInput("equipmentInfoTable", "Choose Equipment Information Table", 
                       choices = selectEquipTable(),
                       options = list(placeholder = "", 
                                      onInitialize = I('function() {this.setValue(""); }')))
      })
      
      
      ### TabPanel: User History
      # Returns user history.
      output$userTable <- renderDataTable({
        validate(need(dbExistsTable(tableDB, "UserHistory"), "Database is empty. No projects exist."))
        if(nchar(input$reviewDB) != 0){
          dbReadTable(tableDB, "UserHistory")
        }
      })
    }
  })
  
  observeEvent(input$fileIdentityTable, ignoreNULL = TRUE, {
    # Returns user selected fileIdentity table.
    output$fileTable <- renderDataTable({
      if(any(grepl("_fileIdentity",dbListTables(tableDB)))){
        if(!is.null(input$fileIdentityTable)){
          name = input$fileIdentityTable
          if(grepl("_fileIdentity", name)){
            dbReadTable(tableDB, input$fileIdentityTable)[, -5]
          }
        }
      }
    })
  })
  
  observeEvent(input$fileCommentTable, ignoreNULL = TRUE, {
    # Returns user selected classification table.
    output$commentTable <- renderDataTable({
      if(any(grepl("_fileComments",dbListTables(tableDB)))){
        if(!is.null(input$fileCommentTable)){
          name = input$fileCommentTable
          if(grepl("_fileComments", name)){
            dbReadTable(tableDB, input$fileCommentTable)
          }
        }
      }
    })
  })
  
  observeEvent(input$markerIdentityTable, ignoreNULL = TRUE, {
    # Returns user selected markerIdentity table.
    output$colTable <- renderDataTable({
      if(any(grepl("_markerIdentity",dbListTables(tableDB)))){
        if(!is.null(input$markerIdentityTable)){
          name = input$markerIdentityTable
          if(grepl("_markerIdentity", name)){
            dbReadTable(tableDB, input$markerIdentityTable)
          }
        }
      }
    })
  })  
  
  observeEvent(input$classificationTable, ignoreNULL = TRUE, {
    # Returns user selected classification table.
    output$classTable <- renderDataTable({
      if(any(grepl("_Classification",dbListTables(tableDB)))){
        if(!is.null(input$classificationTable)){
          name = input$classificationTable
          if(grepl("_Classification", name)){
            dbReadTable(tableDB, input$classificationTable)
          }
        }
      }
    })
  })
  
  observeEvent(input$equipmentInfoTable, ignoreNULL = TRUE, {
    # Returns user selected fileIdentity table.
    output$equipTable <- renderDataTable({
      if(any(grepl("_equipmentInfo",dbListTables(tableDB)))){
        if(!is.null(input$equipmentInfoTable)){
          name = input$equipmentInfoTable
          if(grepl("_equipmentInfo", name)){
            dbReadTable(tableDB, input$equipmentInfoTable)[, -5]
          }
        }
      }
    })
  })
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##### TAB: DELETE DATA #####
  # Get database from server.
  observeEvent(input$deleteDBSelection, ignoreNULL = TRUE, {
    reset("projectChoiceDelete")
    reset("chooseFiles")
    
    # If no database was selected nothing happens.
    if(nchar(input$deleteDBSelection) == 0){
      return(NULL)
    }else{
      # Otherwise a database connection is established.
      #### PATH TO LOAD DATABASE ####
      dbDelete <<- dbConnect(SQLite(), dbname = paste(DB.path, 
                                                      input$deleteDBSelection, ".sqlite3", sep = ""))
      
      # Allows selection of project available in loaded database.
      output$projectChoiceDelete <- renderUI({
        validate(need(length(dbListTables(dbDelete)) > 0, "Database is empty. No projects exist."))
        selectizeInput("projectnameDelete", label = "Select a project you want to edit:", 
                       choices = DBprojects(dbDelete),
                       options = list(placeholder = "", 
                                      onInitialize = I('function() {this.setValue(""); }')))
      })
    }
  })
  
  # Observes selection of project and lists all files within.
  observeEvent(input$projectnameDelete, ignoreNULL = TRUE, {
    output$chooseFiles <- renderUI({return()})
    # Saves chosen projectname to reactiveValues().
    values$projectnameDelete <- as.character(input$projectnameDelete)
    # Only if a project is selected the files within are displayed.
    if(nchar(values$projectnameDelete) > 0) {
      # Those user can delete everything regardless to date of file inclusion.
      if(values$userLogin == "RB" || values$userLogin == "YH" || values$userLogin == "IK"
         || values$userLogin == "AR"|| values$userLogin == "SG"){
        # Displays a drop down menu containing names of files within project. User can select 
        # multiple files.
        output$chooseFiles <- renderUI({
          # Prevention of error messages.
          validate(
            need(dbExistsTable(dbDelete, paste0(values$projectnameDelete, "_fileIdentity")),
                 "Project is corrupt. Meaning no file identification table available. Please select another."))
          validate(
            need(nrow(dbReadTable(dbDelete, paste0(values$projectnameDelete, "_fileIdentity"))) > 0,
                 "Project is corrupt. Meaning file identification table shows no entries. Please select another."))
          
          # Get project_fileIdentity and save it to reactiveValues().
          sql.fileIdentity = paste("select * from ", values$projectnameDelete, "_fileIdentity", 
                                   sep = "")
          values$fileTable <- dbGetQuery(dbDelete, sql.fileIdentity)
          selectInput("selectedFiles", "Choose to be deleted files:", selected = NULL,
                      multiple = TRUE, choices = values$fileTable[, "filename"])
        })
      }else{
        # Get current date and calculate range date.
        currentDate = as.integer(gsub("-", "", Sys.Date()))
        rangeDate = currentDate-7
        
        # Table with all files currently in database.
        UserHistory = dbReadTable(dbDelete, "UserHistory")
        deleted = UserHistory[which(UserHistory[, "Comments"] == 
                                      "The file was removed from all tables of the database."), "File"]
        okFiles = UserHistory[which(!(UserHistory[, "File"] %in% deleted) == TRUE),]
        # Get files of selected project.
        time = okFiles[which(okFiles[,"Project"] == values$projectnameDelete), "Date"]
        
        # Remove time from dates.
        dates = gsub("-", "", substr(time, 1, 10))
        
        # Get all files which were added not more than 7 days ago.
        allFiles = okFiles[which((rangeDate <= as.integer(dates)) == TRUE), "File"]
        
        # Displays a drop down menu containing names of files within project. User can select 
        # multiple files.
        output$chooseFiles <- renderUI({
          validate(
            need(dbExistsTable(dbDelete, paste0(values$projectnameDelete, "_fileIdentity")), 
                 "Project is corrupt. Meaning no file identification table available. Please select another."))
          validate(
            need(nrow(dbReadTable(dbDelete, paste0(values$projectnameDelete, "_fileIdentity"))) > 0,
                 "Project is corrupt. Meaning file identification table shows no entries. Please select another."))
          validate(need(length(allFiles) > 0, 
                        "You have no permission to delete any file of the current project."))
          selectInput("selectedFiles", "Choose to be deleted files:", selected = NULL,
                      multiple = TRUE, choices = allFiles)
        })
      }
    }
  })
  
  # Saves selected files which will be deleted during next step in reactiveValues().
  inputDelete <- eventReactive(input$selectedFiles, {
    values$selectedFiles <- input$selectedFiles
  })
  
  # Displays selected files in 'View Selection'-box simultaneously.
  observeEvent(input$selectedFiles, ignoreNULL = TRUE, {
    output$checkFiles <- renderTable({
      # If a file is selected it will be displayed in table format -> one below the other.
      if(length(inputDelete()) > 0){
        table = NULL
        for(i in 1:length(inputDelete())){
          table = rbind(table, inputDelete()[i])
        }
        table
      }
      # No colnames will be displayed in table.
    }, striped = TRUE, include.colnames = FALSE)
  })
  
  # With button press all input$selectedFiles are removed from tables which include those.
  # Discard button removes table from reactiveValues() and closes pop up.
  observeEvent(input$noSure, ignoreNULL = TRUE, {
    toggleModal(session, "sure", toggle = "close")
  })
  
  observeEvent(input$yesSure, ignoreNULL = TRUE, {
    toggleModal(session, "sure", toggle = "close")
    
    withBusyIndicatorServer("yesSure", {
      # Progressbar to show user app is working.
      withProgress(message = "", value = 0, {
        # Get fileID's of input$selectedFiles from project_fileIdentity.
        fileID = values$fileTable[values$fileTable$filename %in% input$selectedFiles, "file_ID"]
        
        ### Removal of entries in identity tables, the equipment table and the project table itself.
        for(i in 1:length(fileID)){
          ##### PROGRESS MESSAGE
          text = paste("Deleting file ", input$selectedFiles[i], " from project ", 
                       values$projectnameDelete, ". The table will be reindexed to 
                       still allow a fast access. This step takes some time.", sep = "")
          incProgress(1/length(input$selectedFiles), message = "", detail = text)
          ##### PROGRESS MESSAGE
          
          # Queries to delete file by file_ID column.
          sql.deleteFileIdentity = paste("delete from ", values$projectnameDelete, "_fileIdentity where 
                                         file_ID = ", fileID[i], sep = "")
          sql.deleteMarkerIdentity = paste("delete from ", values$projectnameDelete, 
                                           "_markerIdentity where file_ID = ", fileID[i], 
                                           sep = "")
          sql.deleteEquip = paste("delete from ", values$projectnameDelete, 
                                  "_equipmentInfo where file_ID = ", fileID[i], 
                                  sep = "")
          sql.deleteProject = paste("delete from ", values$projectnameDelete, " where file_ID = ", 
                                    fileID[i], sep = "")
          # File is removed from all tables.
          dbGetQuery(dbDelete, sql.deleteFileIdentity)
          dbGetQuery(dbDelete, sql.deleteMarkerIdentity)
          dbGetQuery(dbDelete, sql.deleteEquip)
          dbGetQuery(dbDelete, sql.deleteProject)
        }
        
        # Re-create index on file_ID-column of project table. First old index is dropped than new one 
        # is created. Other queries would slow down the reading later.
        sql.drop = paste("drop index ", values$projectnameDelete, "_idxfile", sep = "")
        dbGetQuery(dbDelete, sql.drop)
        sql.create = paste("create index ", values$projectnameDelete, "_idxfile on ", 
                           values$projectnameDelete, " (file_ID)", sep = "")
        dbGetQuery(dbDelete, sql.create)
        
        
        ### Removal of entries in project_fileComments.
        # Check if any of the selected files is listed in project_fileComments.
        if(dbExistsTable(dbDelete, paste(values$projectnameDelete, "_fileComments", sep = ""))) {
          # Load project_fileComments.
          fileComments = dbReadTable(dbDelete, paste(values$projectnameDelete, "_fileComments", sep = ""))
          if(any(input$selectedFiles %in% fileComments[, "filename"])) {
            # Get filenames which occur in project_fileComments.
            filenames = fileComments[which(fileComments[, "filename"]%in%input$selectedFiles==TRUE),
                                     "filename"]
            # All entries corresponding to filenames will be removed from database table.
            for(i in 1:length(filenames)) {
              sql.deleteComments = paste("delete from ", values$projectnameDelete, "_fileComments where 
                                         filename = '", filenames[i], "'", sep = "")
              # File is removed from table.
              dbGetQuery(dbDelete, sql.deleteComments)
            }
          }
        }
        
        
        ### Removal of entries in project_SPILLmatrix and project_SPILLcolnames.
        # Entries from project_SPILLmatrix and project_SPILLcolnames are removed.
        if(dbExistsTable(dbDelete, paste(values$projectnameDelete, "_SPILLmatrix", sep = ""))) {
          SPILLmatrix = dbReadTable(dbDelete, paste(values$projectnameDelete, "_SPILLmatrix", sep = ""))
          SPILLcolnames = dbReadTable(dbDelete, paste(values$projectnameDelete, "_SPILLcolnames", sep = ""))
          
          # Check if any of those fileID's occur in project_SPILLmatrix (occurence here would 
          # result in entries in project_SPILLcolnames too).
          if(any(fileID %in% SPILLmatrix[, "file_ID"])) {
            # Get file_ID's which occur in project_SPILLmatrix.
            fileID = unique(SPILLmatrix[SPILLmatrix$file_ID %in% fileID, "file_ID"])
            # In the following the entries with file_ID of input$selectedFiles are removed 
            # from project_SPILLmatrix and project_SPILLcolnames.
            for(i in 1:length(fileID)) {
              sql.deleteSPILLmatrix = paste("delete from ", values$projectnameDelete, 
                                            "_SPILLmatrix where file_ID = ", fileID[i], 
                                            sep = "")
              sql.deleteSPILLcolnames = paste("delete from ", values$projectnameDelete, 
                                              "_SPILLcolnames where file_ID = ", fileID[i], 
                                              sep = "")
              # File is removed from all tables.
              dbGetQuery(dbDelete, sql.deleteSPILLmatrix) 
              dbGetQuery(dbDelete, sql.deleteSPILLcolnames) 
            }
          }
        }
        
        ### Editing UserHistory.
        comment = "The file was removed from all tables of the database."
        tempUserHistory = data.frame(Date = rep(as.character(Sys.time()), 
                                                length(input$selectedFiles)), 
                                     User = rep(values$userLogin, length(input$selectedFiles)), 
                                     Project = rep(values$projectnameDelete, length(input$selectedFiles)), 
                                     File = input$selectedFiles, 
                                     Comments = rep(comment, length(input$selectedFiles)))
        dbWriteTable(dbDelete, "UserHistory", tempUserHistory, append = TRUE)
        })
      })
    
    output$checkFiles <- renderTable({return()})
    
    reset("projectChoiceDelete")
    reset("chooseFiles")
    })
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##### TAB: How to... #####
  output$videoUpload <- renderUI({
    h4("Tutorial - Uploading Data", br(), tags$video(src = paste0(App.path, "www/23.mp4"), type = "video/mp4",
                                                     width = "350px", height = "350px", controls = "controls"))
  })
  
  output$videoPlot <- renderUI({
    h4("Tutorial - Plotting Data", br(), tags$video(src = paste0(App.path, "www/videoUpload.mp4"), type = "video/mp4",
                                                    width = "350px", height = "350px", controls = "controls"))
  })
  
  output$videoReview <- renderUI({
    h4("Tutorial - Reviewing Data", br(), tags$video(src = paste0(App.path, "www/23.mp4"), type = "video/mp4",
                                                     width = "350px", height = "350px", controls = "controls"))
  })
  
  output$videoDelete <- renderUI({
    h4("Tutorial - Deleting Data", br(), tags$video(src = paste0(App.path, "www/videoUpload.mp4"), type = "video/mp4",
                                                    width = "350px", height = "350px", controls = "controls"))
  })
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  #####  TAB: IMPRESSUM #####
  # Gets input from seperate file.
  output$Impressum <- renderUI({
    includeHTML(paste0(App.path, "www/Impressum.html"))
  })
  
  # Display of DRFZ logo.
  output$DRFZlogo <- renderImage({
    return(list(
      src = paste0(App.path, "www/Images/DRFZlogo.jpg"),
      filetype = "image/jpg"
    ))
  }, deleteFile = FALSE)
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  #####  CLOSE APP #####
  # Function to disconnect database connection, clear workspace and close App and browser 
  # window.
  observeEvent(input$yes, ignoreNULL = TRUE, {
    # Connection in tab 'Uploading data'.
    if(exists("DB")){
      # Disconnects database.
      dbDisconnect(DB)
    }
    
    # Connection in tab 'Reviewing Tables'.
    if(exists("tableDB")){
      # Disconnects database.
      dbDisconnect(tableDB)
    }
    
    # Clears workspace.
    rm(list=ls())
    # Closes browser window and stops app.
    js$closeWindow()
    stopApp()
  })
  
  })


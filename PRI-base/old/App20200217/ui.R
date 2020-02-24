### ui.R
# Needed to quit App at the end. (Later in combination with Logout.)
jscode <- "shinyjs.closeWindow = function() {window.close();}"
jsResetCode <- "shinyjs.refresh = function() {history.go(0)}"

source(file.path(App.path,"www/helpers.R"))


header <- dashboardHeader(titleWidth = 420)
anchor <- tags$a(target="_blank", href="https://www.drfz.de/en/",
                 tags$img(src="drfz.png", height = "90", hspace="20"),
                 "PRIbase")

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: white }"))),
  anchor,
  class = 'name')

loginModal=function(id,label,trig,usr,pwd,buttonID){
  bsModal(id,label,trigger=trig,size="small",
          tags$head(tags$style(paste0("#",
                                      id,
                                      " .modal-footer{display:none}"))
          ),
          textInput(usr,"Username or Email:"),
          passwordInput(pwd, "Password:"),
          br(),br(),
          div(withBusyIndicatorUI(actionButton(buttonID, "Login",icon=icon("sign-in"))),
              style="text-align: center;"),
          br(),
          div(helpText("Press ESC to cancel Login"),
              style="text-align: center;"),
          br()
  )
}

dashboardPage(
  # Color theme of dashboard.
  skin="black",
  header,
  # Title shown in browser tab.
  title = "PRIbase",
  
  # Menu sidebar including upload, show identity tables and help tab as well as the 'Quit App'-
  # button.
  dashboardSidebar(
    sidebarMenu(
      br(),br(),
      
      conditionalPanel(
        condition = "input.closeLOGIN",
        textOutput("userpanel"),
        actionButton("quit", "Logout", icon = icon("sign-out"))
      ),
      br(), br(),
      menuItem("Uploading Data", tabName = "upload", icon = icon("upload")),
      menuItem("Downloading Data", tabName = "download", icon = icon("download"),
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Plotting Data", tabName = "plot", icon = icon("area-chart")),
      menuItem("Reviewing Data", tabName = "review", icon = icon("table")),
      menuItem("Deleting Data", tabName = "delete", icon = icon("remove")),
      menuItem("How to...", tabName = "help", icon = icon("question")),
      br(),
      br(),
      menuItem("Impressum", tabName = "imp", icon = icon("info"))
    )),
  
  # Main body. Content of tabs defined in dashboardSidebar(). 'style' ensures that long drop-down lists
  # are still completely visible regardless to window size.
  dashboardBody(style = "overflow-y:scroll; max-height:2000px; min-height:700px; position:relative;",
# Loads stylesheet for App. (Same folder as server.R and ui.R!)
includeCSS("www/style.css"),
# Allows shinyjs functions to work in app.
useShinyjs(),
# Add the js-code to the page.
extendShinyjs(text = jsResetCode),

# Loading message
div(
id = "loading-content",
h2("Loading...")
),

# The main app code goes here
hidden(
div(
id = "app-content",

tabItems(
##################################################################################################
##################################################################################################
##################################################################################################
##### TAB: UPLOADING DATA #####
# First tab contents.
tabItem(tabName = "upload",
      # Allows to close tab with 'Quit App' button.
      extendShinyjs(text = jscode, functions = c("closeWindow")),
      
      #### POP-UP: PROJECT ####
      # Warns user that no leading numbers are allowed in project name.
      bsModal("warningProject", "Warning", trigger = "", size = "small",
              tags$head(tags$style("#warningProject .modal-footer{display:none}")),
              helpText("No leading numbers for project names allowed. The table name will clash 
                       with database."),
              br(),
              actionButton("closeProject", "Close"),
              
              br(), br()),
      
      #### POP-UP: USERNAME ####
      # User provides initials. As soon as login is established: not needed anymore.
      bsModal("login", "Provide Username Initials", trigger = "", size = "small",
              tags$head(tags$script(HTML(jscode))),
              tags$head(tags$style("#login .modal-footer{display:none}")),
              textInput("username", label = NULL, placeholder = "Initials like JD"),
              br(),
              actionButton("closeLOGIN", "Close"),
              br(), br()),
      
      #### POP-UP: CLASSTABLE WARNING ####
      bsModal("classTableExists", "Information already provided", trigger = "", 
              size = "small",
              tags$head(tags$style("#classTableExists .modal-footer{display:none}")),
              textOutput("noteClassTable"),
              br(),
              actionButton("note", "Close"),
              br(), br()),
      
      #### POP-UP: INVALID FILENAME ####
      # Warns user if chosen path was not read correctly. e.g. due to invalid characters
      # in filename or folder.
      bsModal("invalidFilenames", "Invalid Filenames", trigger = "", size = "large",
              textOutput("invalidFiles"),
              tableOutput("invalidTable"),
              br()),
      
      #### POP-UP: USER HISTORY ####
      # Opens pop up.
      bsModal("userHistory", title = "User history", trigger = "", size = "large",
              tags$head(tags$style("#userHistory .modal-footer{display:none}")),
              fluidRow(
                tags$div(class = "helpTextMargin",
                         helpText(HTML("<p><b>Note:</b> Please provide some information 
                                       concerning the current session. The information 
                                       will be saved in the user history table. (optional)</p>"))),
                column(6, textInput("commentsUser", label = "Comments"))),
              # Adds user-provided information to user history table.
              br(),
              actionButton("addButton2", "Show table"),
              br(), br(), br(),
              # Grey line which divides input part from actual table.
              column(12, style = "background-color:lightgrey", div(style = "height:5px;")),
              br(), br(),
              # Shows information of current session.
              dataTableOutput("UserHistory"),
              br(), br(),
              # Displays button which closes the modal window. Shows up when addButton2 is 
              # clicked.
              div(id = "closeModalUserHistory",
                  helpText(HTML("<p align='right'><b>Note:</b> Please check the information before 
                                <br> you close the window and save it.</p>")),
                  actionButton("closeModalUSER", "Close")),
              br(), br()),
      
      #### POP-UP: SHORTNAME TABLE ####
      # MarkerIdentity table displayed in pop up window. User can decide if he wants to keep or
      # discard the table.
      bsModal("markerNamesWindow", title = "Marker Table", 
              trigger = "decision", size = "large",
              tags$head(tags$style("#markerNamesWindow .modal-footer{display:none}")),
              helpText(HTML("<p>The shortnames are needed for plotting later on. The names will be read from 
                            file header and order alphabetically. Before the table is saved it will be shown 
                            for inspection.</p>")),
              # While calculation is running, a spinning circle appears so that user knows
              # app is running.
              div(id = "loading-wheel", HTML("<i class='fa fa-spinner fa-pulse fa-3x fa-fw'></i>
                                             <span class='sr-only'>Loading...</span>")),
              # Manual input displays textInput() depending on ncol() of uploaded files.
              tags$div(id = "manualBox",
                       div(id = "textcolor", textOutput("diffFilesText")),
                       div(id = "textcolor", textOutput("oddFilesText")),
                       br(),
                       column(5, uiOutput("shortnameInput")),
                       column(7, 
                              actionButton("showColTable", "Print Table"),
                              br(), br(),
                              tableOutput("markerTable")),
                       br(), br(),
                       # Grey line which divides table from 'Keep/Discard'-box.
                       column(12, style = "background-color:lightgrey", div(style = "height:5px;")),
                       
                       conditionalPanel(
                         condition = "input.showColTable",
                         br(),br(),
                         # Buttons to include or discard markerNames file.
                         tags$div(id = "manDiscardBox",
                                  box(status = "danger",
                                      helpText(HTML("<p>Would you like to keep the table?</p>")),
                                      # Modal window closes and discards markerNamesTable.
                                      actionButton("manKeep", "Keep"),
                                      # Modal window closes and keeps markerNamesTable.
                                      actionButton("manDiscard", "Discard")))))),
      
      fluidRow(
        # User gets the choice to load or create a new database.
        column(3,
               div(id = "databaseDecision",
                   helpText("Would you like to create or load a new database?"),
                   actionButton("new", "New"),
                   actionButton("load", "Load")),
               hidden(div(id = "restartWidgets",
                          helpText("To start over click below."),
                          actionButton("restartButton", "Refresh Session", 
                                       icon = icon("refresh")))),
               br(), br(),
               # Drop down with path to all databases.
               conditionalPanel(
                 condition = "input.load",
                 selectizeInput("dbSelection", "Choose a Database:", 
                                choices = sub(".sqlite3", "", list.files(DB.path, pattern = "sqlite3$")),
                                options = list(placeholder = "", 
                                               onInitialize = I('function() {this.setValue(""); }')))),
               
               
               #### The following shows up if user decided to create a new database. ####
               
               # The following creates a new empty database with user-chosen name.
               # To ensure standardized structure of database names user is asked to provide:
               # his intials, database content, intials of experimenter, and date of 1st measurement.
               # Further information can be given in 'Additional Information', e.g. gated CD44.
               bsModal("newDBWindow", title = "Name New Database", trigger = "new", size = "small",
                       tags$head(tags$style("#newDBWindow .modal-footer{display:none}")),
                       textInput("initialExperimenter", "Initials of Experimenter*"),
                       dateInput("measureDate", "Date of 1st Measurement"),
                       textInput("dbContent", "Content of Database*"),
                       textInput("addInformation", "Additional Information", placeholder = "e.g. gCD44"),
                       # The button compiles the name (format: XX_xxxxx_XX_xxxxx_XXXXXXXX.sqlite3)
                       actionButton("saveDB", "Compile Name"),
                       actionButton("cancelDB", "Cancel"),
                       hidden(actionButton("closeDB", "Close")),
                       br(), 
                       helpText(HTML("<p style='color:darkgrey; font-size:11px;'>
                                     Example: JD_20170101_Cytokines_gatedCD44.sqlite3</p>")),
                       br(),
                       # Display of database string.
                       uiOutput("infoDBName"),
                       textOutput("newDBName"),
                       br()),
               
               # Modal window with warning if user forgot to provide some needed information in create new DB
               # window.
               bsModal("modalDBWarning", "Warning", trigger = "", size = "small",
                       tags$head(tags$style("#modalDBWarning .modal-footer{display:none}")),
                       textOutput("DBWarning"),
                       # Button closes warning window.
                       actionButton("closeDBWarning", "Close"),
                       br(), br()),
               
               hidden(div(id = "widgetsNewDB",
                          # Tells user how to proceed after the decision of creating a new database.
                          helpText(HTML("<b>How to create a new database:</b>
                                        <ol> 
                                        <li><p align='justify'>'New' opens window with standarized database namin layout. 
                                        'Compile Name' creates database, 'Cancel' terminates database naming. 'Close' establishes
                                        connection with new database.</p></li>
                                        <li><p align='justify'>Within the 'Upload Data'-box name the new project.</p></li>
                                        <li><p align='justify'>Please provide information ('Add experiment information') concerning 
                                        the new project.</p></li>
                                        <li><p align='justify'>'Select Files' which should be included to the project.</p></li>
                                        <ul>                                       
                                        <li><p align='justify'>Files can be commented ('Comment Files').</p></li>
                                        </ul>
                                        <li><p align='justify'>Complementary: Quote staining information of chosen files if 
                                        necessary.</p></li>
                                        <li><p align='justify'>'Marker Names' opens a pop up window. A table including 
                                        marker names of the uploaded files can be uploaded or markers can be named manually. Changes 
                                        can be kept or discarded.</p></li>
                                        <li><p align='justify'>Following, compensation of data can be selected.</p></li>
                                        <li><p align='justify'>As soon as project name, files and marker names are provided 
                                        the 'Run'-button is enabled and data can be included in the project table.
                                        </p></il></ol>")),
                          br(),br())),
               
               
               #### The following shows up if user decided to 'Load' a database. ####
               hidden(
                 # User decides whether he wants to extend a project or create a new one. 
                 selectizeInput("choiceProject", label = "Extend a project or create a new one?", 
                                choices = list("Load", "New"),
                                options = list(placeholder = NULL, 
                                               onInitialize = I('function() {this.setValue(""); }'))),
                 
                 br()),
               hidden(div(id = "widgetsLoadDB",
                          # Tells user how to proceed after the decision of loading a database.
                          helpText(HTML("<b>How to extend an existing database:</b>
                                        <ol> 
                                        <li><p align='justify'>Choose the database to be extended.</p></li>
                                        <li><p align='justify'>Decide whether you want to edit an existing project or create a new one 
                                        within the chosen database. Any choice opens the 'Upload Data'-box.</p></li>
                                        <ul type='square'>
                                        <li><p align='justify'>'Load' allows to select any project.</p></li>
                                        <li><p align='justify'>'New' asks for a name of the new project.</p></li>
                                        <ul>                                       
                                        <li><p align='justify'>Please provide information ('Add experiment information') concerning  
                                        the new project.</p></li>
                                        </ul>
                                        </ul>
                                        <li><p align='justify'>'Select Files' to be added to the project.</p></li>
                                        <ul>                                       
                                        <li><p align='justify'>Files can be commented ('Comment Files').</p></li>
                                        </ul>
                                        <li><p align='justify'>Complementary: Quote the staining information of your chosen files.</p></li>
                                        <li><p align='justify'>'Marker Names' opens a pop up window. A table including 
                                        marker names of the uploaded files can be uploaded or markers can be named manually. Changes 
                                        can be kept or discarded.</p></li>
                                        <li><p align='justify'>Following, compensation of data can be selected.</p></li>
                                        <li><p align='justify'>As soon as project name, files and marker names are provided  
                                        the 'Run'-button is enabled and data can be included in the project table.
                                        </p></il></ol>")),
                          br(), br()))),
        
        #### 'UPLOAD DATA'-BOX ####
        column(9,
               conditionalPanel(
                 # Either choice opens the 'upload Data'-box.
                 condition = "input.choiceProject == 'Load'|| input.choiceProject == 'New' ||  input.closeDB",
                 box(title = "Upload Data", solidHeader = TRUE, status = "info", collapsible = FALSE,
                     collapsed = FALSE, width = 12,
                     fluidRow(
                       column(6,
                              conditionalPanel(
                                condition = "input.choiceProject == 'Load'",
                                helpText(HTML("<p align='justify'><b>Note:</b> Please choose one of the existing projects you
                                              would like to edit.</p>"))),
                              conditionalPanel(
                                condition = "input.choiceProject == 'New' || input.new",
                                helpText(HTML("<p align='justify'><b>Note:</b> Please enter the name of the new project to be added 
                                              to the database.</p>"))),
                              # Allows user to choose an existing project or enter a new one.
                              tags$div(style="display:inline-block; width:95%", uiOutput("projectChoice")),
                              # Compiles 'info circle' next to widget input$projectChoice.
                              HTML("<i id='infoCircle' class='fa fa-info-circle'></i>"),
                              br(),
                              # Button shows up if classification table for current table does not exist yet.
                              actionButton("classificationButton", "Project Information"),
                              br(), br(),
                              # 'Classification' modal window.
                              bsModal("classificationWindow", title = "Classification Table", trigger = "", 
                                      size = "large",
                                      tags$head(tags$style("#classificationWindow .modal-footer{display:none}")),
                                      fluidRow(
                                        # HelpText position needed to be changed but only here. Therefore a new class
                                        # was assigned in div tag. The adjustments were made within css file.
                                        tags$div(class="helpTextMargin",
                                                 helpText(HTML("<p><b>Note:</b> Please provide information concerning the current project.
                                                               All marked fields should be completed. If you do not have the 
                                                               information <br> write 'UNKNOWN' or 'NONE'.</p>"))),
                                        # Interactive fields to be filled out by user.
                                        column(4,
                                               dateInput("date", label = "Start of project*"),
                                               textInput("experimenter", label = "Experimenter*", placeholder = "e.g. John Doe"),
                                               textInput("expType", label = "Type of experiment*", placeholder = "e.g. In vivo"),
                                               textInput("species", label = "Species*", placeholder = "e.g. Mouse"),
                                               textInput("subspecies", label = "Subspecies*", placeholder = "e.g. NZBxW F1")),
                                        column(4,
                                               textInput("gender", label = "Gender", placeholder = "e.g. Female"),
                                               textInput("tissue", label = "Tissue*", placeholder = "e.g. Spleen"),
                                               textInput("celltype", label = "Celltype*", placeholder = "e.g. T cells"),
                                               textInput("subpopulation", label = "Subpopulation*", placeholder = "e.g. all CD4+ and CD8+ T cells")),
                                        column(4,
                                               textInput("stimulation", label = "Stimulation*", placeholder = "e.g. Unstimulated"),
                                               textInput("duration", label = "Duration of Stimulation*", placeholder = "e.g. 0 h"),
                                               textInput("staining", label = "Staining information*", placeholder = "e.g. Surface"),
                                               textInput("comments", label = "Comments"))),
                                      # Action button allows user to check his provided information...
                                      actionButton("addButton1", "Show table"),
                                      br(), br(),
                                      # Grey line which divides input part from actual table.
                                      column(12, style = "background-color:lightgrey", div(style = "height:5px;")),
                                      br(), br(),
                                      # Shows user to be added table with his information.
                                      dataTableOutput("classification"),
                                      br(),
                                      # ...before the information is included in the classification table of the database.
                                      # The button closes the modal window and saves the information. Shows up when
                                      # addButton1 is clicked.
                                      conditionalPanel(
                                        condition = "input.addButton1",
                                        uiOutput("helpText"),
                                        actionButton("closeClassification", "Close")),
                                      br(), br()),
                              
                              # Modal window with warning if user forgot to provide some needed information in classification
                              # window.
                              bsModal("modalClassWarning", "Warning", trigger = "",
                                      textOutput("classWarning"),
                                      tags$head(tags$style("#modalClassWarning .modal-footer{display:none}")),
                                      HTML("<button id='closeWarning' type='button' data-dismiss='modal' 
                                           class='btn'>Close</button>")),
                              
                              # Selection of added files. Help text is justified (format).
                              helpText(HTML("<p align='justify'><b>Note:</b> You can upload several FACS files (.fcs)
                                            at once. But keep in mind to only choose files with the same markers. If 
                                            you would like to comment some of your uploaded files press 'Comment Files'.  
                                            You can comment any file within the project later on too.</p>")),
                              div(id="centerBUTTONS",
                                  actionButton("files", "Select Files"),
                                  actionButton("commentFiles", "Comment Files")),
                              textOutput("summaryFileIdentity"),
                              
                              # Window pops up if 'Comment Files' - button is clicked.
                              bsModal("commentsWindow", title = "Comment Files", trigger = "commentFiles", 
                                      size = "large",
                                      tags$head(tags$style("#commentsWindow .modal-footer{display:none}")),
                                      fluidRow(
                                        # Checkbox including all selected files where user can choose multiple files 
                                        # to comment.
                                        column(5, uiOutput("checkboxFiles")),
                                        # Comment line for chosen files.
                                        column(7, textInput("fileComments", "Please comment the chosen files", 
                                                            placeholder = "file was merged"))),
                                      # Button to add information to fileComments table.
                                      fluidRow(
                                        actionButton("addButton3", "Show table"),
                                        actionButton("cancelComment", "Cancel")),
                                      br(), br(), br(),
                                      # Button to remove latest entry. Shows up as soon user added comments to table.
                                      conditionalPanel(
                                        condition = "output.fileCommentsTable",
                                        helpText(HTML("<p align='right'><b>Note:</b> This button removes the latest 
                                                      <br>input from the table.</p>")),
                                        actionButton("removeComment", "Remove Entry"),
                                        br(), br(),
                                        # Grey line which divides input part from actual table.
                                        column(12, style = "background-color:lightgrey", div(style = "height:5px;")),
                                        br(), br()),
                                      # Shows user to be added table with his information.
                                      dataTableOutput("fileCommentsTable"),
                                      br(),
                                      # The button closes the modal window and saves the information. Shows up when
                                      # table is rendered.
                                      conditionalPanel(
                                        condition = "output.fileCommentsTable",
                                        uiOutput("helpTextComments"),
                                        actionButton("closeComments", "Close")),
                                      br(),br()),
                              br(), br(),
                              # Input of staining information of selected files. Output of text showing which files were uploaded.
                              # 'id' needed to reset the widget after file upload. 
                              helpText(HTML("<p align='justify'><b>Note:</b> Enter common staining information of  
                                            chosen files. (optional)</p>")),
                              textInput("stainInfo", label = NULL, placeholder = "e.g. Surface")),
                       
                       column(6,
                              # Selection of marker table.
                              helpText(HTML("<p align='justify'><b>Note:</b> You can either upload a prepared table 
                                            or load marker names from file and edit them manually.</p>")),
                              tags$div(style="display:inline-block",title="Select files first",
                                       actionButton("decision", "Marker Names")),
                              br(), br(), br(),
                              
                              # Decision whether to compensate files or not.
                              tags$div(style="display:inline-block; width:36hr", radioButtons("compensate", label = "Shall the data be compensated?", 
                                                                                              choices = list("Yes" = 1, "No" = 2), selected = 2, inline = TRUE)),
                              # Compiles 'info circle' next to widget input$compensate.
                              HTML("<i id='compInfoCircle' class='fa fa-info-circle'></i>"),
                              br(), br(),
                              # Allows data inclusion in database.
                              column(12,
                                     # To customize box position a new class was assigned to the following one.
                                     tags$div(class="includeBox",
                                              box(title = "Include Data", solidHeader = TRUE, status = "info",
                                                  width=20,
                                                  helpText(HTML("<p align='justify'><b>Note:</b> Make sure uploaded data is correct before 
                                                                you include it in the database.</p>")),
                                                  # With click identity tables are updated and fcs files are included in project table. 
                                                  # Button is disabled and tells user what is
                                                  # needed for functioning.
                                                  tags$div(style = "display:inline-block", title = "Project, files and marker names needed.",
                                                           actionButton("updateProject", "Run")),
                                                  br(), br(), br(),
                                                  textOutput("summaryProject"),
                                                  br())))))),
                 
                 # Display of chosen Files.
                 fluidRow(
                   hidden(div(id="chosenBox", 
                              box(title = "Display of Chosen Files", solidHeader = TRUE, status = "info", 
                                  collapsible = FALSE, collapsed = FALSE, width = 9,
                                  # Returns number of included files as well as the dimensions
                                  # of the database.
                                  textOutput("summaryFiles"),
                                  tableOutput("chosenTable"),
                                  div(id = "textcolor",
                                      # Information concerning files with distinct column quantity.
                                      textOutput("diffFiles"),
                                      tableOutput("diffTable"),
                                      # Information concerning files with distinct marker names.
                                      textOutput("oddFiles"),
                                      tableOutput("oddTable"),
                                      # Information concerning double files.
                                      textOutput("excFiles"),
                                      tableOutput("excTable")))))))))),
#################################################################################################
##################################################################################################
##################################################################################################
##### TAB: DOWNLOAD DATA #####

tabItem(tabName="download",
      fluidRow(
        tags$style("#shiny-notification-premfail {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-commfail {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-upflowtrue {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-upflowfalse {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-manfail {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-infofail {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-appfail {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-pmidfail {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-numericfail {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-keyfail {position: fixed; top: 100px; right: 30px;}"),
        tags$style("#shiny-notification-keytextfail {position: fixed; top: 100px; right: 30px;}"),
        
        #--------------------------------------------------------------------------------------------------
        
        
        conditionalPanel(
          "input.tabset1 == 'Community Cytobank') && (account$com.log==F)",
          loginModal("loginc",
                     "Community Cytobank",
                     "",
                     "com.usr",
                     "com.pwd",
                     "community"
          )
        ),
        
        #--------------------------------------------------------------------------------------------------
        
        conditionalPanel(
          "input.tabset1 === 'Premium Cytobank') && (account$pre.log==F)",
          loginModal("loginp",
                     "Premium Cytobank",
                     "",
                     "pre.usr",
                     "pre.pwd",
                     "premium"
          )
        ),
        
        #--------------------------------------------------------------------------------------------------
        
        tabBox(
          title=tagList(shiny::icon("database"),"Database Server"),
          # The id provides use of input$tabset1 on the server to find the current tab
          id="tabset1",
          side="right",
          width=12,
          selected="FlowRepository",
          tabPanel("Community Cytobank",
                   div(img(src="Cytobank.png",height=50,width=150),
                       style="text-align: center;"),
                   helpText("Click on the header to sort a column"),
                   DT::dataTableOutput("comm"),
                   verbatimTextOutput("communityc")
          ),
          
          tabPanel("Premium Cytobank",
                   div(img(src="Cytobank.png",height=50,width=150),
                       style="text-align: center;"),
                   helpText("Click on the header to sort a column"),
                   DT::dataTableOutput("prem"),
                   verbatimTextOutput("premiump"),
                   div(actionButton("manualprem",
                                    "Enter IDs",icon=icon("hand-stop-o")),
                       style="text-align: center;"),
                   bsModal("mantab","Enter experiment information manually",
                           "manualprem",size="large",
                           box(
                             width=8,
                             DT::dataTableOutput("nulltable"),
                             br(),
                             div(actionButton("Del_row_head","delete",icon=icon("minus-circle")),
                                 style="text-align: center;")
                           ),
                           box(
                             width=4,
                             textInput("ID","Enter experimnet ID"),
                             textInput("researcher","Enter name of researcher"),
                             textInput("title","Enter experiment title"),
                             br(),
                             div(actionButton("addrow","add",icon=icon("plus-circle")),
                                 style="text-align: center;")
                           ),
                           helpText("Fill out text fields to enable download button"),
                           br(),
                           withBusyIndicatorUI(actionButton("mandown","Download",icon=icon("download")))
                   )
          ),
          tabPanel("FlowRepository",
                   div(img(src="FlowRepository.png",height=50,width=150),
                       style="text-align: center;"),
                   helpText("Click on the header to sort a column"),
                   DT::dataTableOutput("flow"),
                   verbatimTextOutput("flowrep"),
                   br(),
                   div(style="display:inline-block",
                       actionButton("FileInfo","Info",
                                    icon=icon("info-circle"))
                   ),
                   div(style="display:inline-block",
                       withBusyIndicatorUI(actionButton("updateflow","Update",
                                                        icon=icon("refresh")))
                   ),
                   div(style="display:inline-block;position:absolute;right:9.3em;",
                       actionButton("enterkey","Add keyword",
                                    icon=icon("plus-circle"))
                   ),
                   div(style="display:inline-block;position:absolute;right:2em;",
                       actionButton("enterPMID","Add PMID",
                                    icon=icon("plus-circle"))
                   ),
                   bsModal("pub","Experiment information","",size="large",
                           htmlOutput("filetext")
                   ),
                   bsModal("addPMID","Add PubMed ID","",size="small",
                           tags$head(tags$style("#addPMID .modal-footer{display:none}")),
                           textInput("PMID","PubMed ID:"),
                           br(),
                           div(style="display:inline-block;position:absolute;left:2em;",
                               actionButton("discard","cancel",icon=icon("close"))),
                           withBusyIndicatorUI(div(style="display:inline-block;position:absolute;right:2em;",
                                                   actionButton("savePMID",
                                                                "save",
                                                                icon=icon("save")))
                           ),
                           br(),br()
                   ),
                   bsModal("addkey","Add keyword","",size="small",
                           tags$head(tags$style("#addkey .modal-footer{display:none}")),
                           textInput("keyword","Keyword:"),
                           br(),
                           div(style="display:inline-block;position:absolute;left:2em;",
                               actionButton("discardkey","cancel",icon=icon("close"))),
                           withBusyIndicatorUI(div(style="display:inline-block;position:absolute;right:2em;",
                                                   actionButton("savekey",
                                                                "save",
                                                                icon=icon("save")))
                           ),
                           br(),br()
                   )
          )
        ),
        
        #--------------------------------------------------------------------------------------------------
        
        box(
          width=12,
          helpText("Select experiment(s) of interest from table to enable download button"),
          withBusyIndicatorUI(actionButton("getfiles","Download",icon=icon("download")))
        )
      )
),
#################################################################################################
##################################################################################################
##################################################################################################
##### TAB: PLOTTING DATA #####            
# Second tab contents.
tabItem(tabName = "plot",
      fluidRow(
        #### POP-UP: CUTOFF WARNING ####
        # Asks User for another cutoff if chosen one is NULL or 0.
        bsModal("warningCutoff", title = "Warning message", trigger = "", size = "small",
                tags$head(tags$style("#warningCutoff .modal-footer{display:none}")),
                textOutput("missingCutoffs"),
                br(),
                actionButton("closeCutoff", "Close")),
        column(4,
               tabBox(
                 # 'Right' puts tabs on the right hand side and reverses them in order.
                 title = "Plot settings",
                 id = "PLOTtabset", side = "right", width = 12,
                 selected = "Plot Files",
                 
                 ### Searching for markers.
                 tabPanel("Search for Marker",
                          helpText("The following allows the search for specific marker names in all 
                                   available databases. The tool searches each files within the selected 
                                   databases for chosen markers. Only files which display all selected 
                                   markers are shown."),
                          # Allows selection of multiple databases.  
                          uiOutput("dbMultiSearch"),
                          br(),
                          
                          # If DB selected, show project choice buttons.
                          conditionalPanel(
                            condition = "input.searchMultiDB",
                            # "Done with selection" -button.
                            helpText("As soon as you are done with your selection, click 'Proceed'."),
                            div(id = "centerBUTTONS", actionButton("done","Proceed")),
                            br(), br()),
                          uiOutput("multiDBprojects"),
                          br(),
                          uiOutput("availableMultiMarker"),
                          # To render search table, button needs to be clicked. Shows up as soon as a marker is selected.
                          conditionalPanel(
                            condition = "input.searchMultiMarker",
                            br(),
                            div(id = "centerBUTTONS", actionButton("createMultiSearch", "Render table")),
                            bsTooltip("createMultiSearch", 
                                      "Returns a file overview considering the search parameters."))),
                 
                 ### Plotting data.
                 tabPanel("Plot Files", 
                          uiOutput("plotDatabases"),
                          br(), br(),
                          
                          
                          conditionalPanel(
                            condition = "input.plotDBSelection",
                            # Project selection.
                            uiOutput("selectProject")),
                          div(id = "plot-content",
                              uiOutput("selectFile"),
                              br(),
                              div(id = "textcolor", textOutput("emptyProject")),
                              br(),
                              conditionalPanel(
                                condition = "input.plotFile",
                                # User decides value type for plotting.
                                radioButtons("calcMethod", label = "Plot Mean Fluorescence Intensity (MFI), 
                                             frequencies or density?", choices = list("MFI" = 1, 
                                                                                      "Frequency" = 2,
                                                                                      "Density" = 3), 
                                             selected = 1, inline = TRUE),
                                br(),
                                # Three drop down where user decides on variables to be plotted and three
                                # cutoffs linked to chosen variables.
                                fluidRow(column(8,uiOutput("selectX")),
                                         column(4,numericInput("xCutoff", "Cutoff x-axis", min = 0, value = 0))),
                                fluidRow(column(8,uiOutput("selectY")),
                                         column(4,numericInput("yCutoff", "Cutoff y-axis", min = 0, value = 0))),
                                fluidRow(column(8,uiOutput("selectLegend")),
                                         column(4,numericInput("zCutoff", "Cutoff z-axis", min = 0, value = 0))),
                                # Reminder for user.
                                bsTooltip("xCutoff", "Provide value greater than 0 and a cutoff for y-axis."),
                                bsTooltip("yCutoff", "Provide value greater than 0 and a cutoff for x-axis."),
                                bsTooltip("zCutoff", "Provide value greater than 0."),
                                
                                # Button plotts graphic.
                                div(id = "centerBUTTONS", withBusyIndicatorUI(actionButton("plotGraphic", "Create Graphic")))))))),
        
        column(8, 
               tabBox(
                 # 'Right' puts tabs on the right hand side and reverses them in order.
                 title = "Results",
                 id = "PLOTtabset", side = "right", width = 16, height = "600px",
                 selected = "Plot data",
                 
                 ### Searching for markers.
                 tabPanel("Search table",
                          tableOutput("searchTable"),
                          br(),
                          conditionalPanel(
                            condition = "input.createMultiSearch",
                            actionButton("downloadSearch", "Download", icon = icon("download")))),
                 tabPanel("Plot data",
                          plotOutput("plot", inline = TRUE),
                          plotOutput("hidePlot", inline = TRUE)))))),


##################################################################################################
##################################################################################################
##################################################################################################
##### TAB: REVIEWING TABLES #####
# Second tab contents.
tabItem(tabName = "review",
      fluidRow(
        # User selects database and all marker and file identity tables can be viewed.
        column(3, helpText(HTML("<p align='justify'> Database tables can be viewed. Choose database with 
                                tables of interest. Rendered tables can be searched by single column (blue
                                highlighted) or across the whole table (yellow highlighted).</p>")),
               br(),
               selectizeInput("reviewDB", "Choose a Database:", 
                              choices = sub(".sqlite3", "", list.files(DB.path, pattern = "sqlite3$")),
                              options = list(placeholder = "", 
                                             onInitialize = I('function() {this.setValue(""); }')))),
        column(6, imageOutput("tableLayout", width = "400px", height = "200px"))),
      
      # Allows user to convert a table from database to .csv and save it locally.
      conditionalPanel(
        br(), br(),
        condition = "input.reviewDB",
        radioButtons("downloadChoice", "First, select a table from the boxes below. Then 
                     decide which to download.",
                     choices = c("File Identification" = 1,
                                 "File Commentary" = 2,
                                 "Marker Identification" = 3,
                                 "Experiment Classification" = 4,
                                 "Equipment Information" = 5,
                                 "User History" = 6), selected = 6, inline = TRUE),
        withBusyIndicatorUI(actionButton("downloadTable", "Download", icon = icon("download"))),
        br(), br(),
        tabBox(
          title = "", width = 12,
          tabPanel("File Identification",
                   # Allows user to load fileIdentity table of database.
                   uiOutput("fileTableChoice"),
                   dataTableOutput("fileTable")),
          tabPanel("File Comments",
                   # Allows user to load fileComments table of database.
                   uiOutput("commentTableChoice"),
                   dataTableOutput("commentTable")),
          tabPanel("Marker Identification",
                   # Allows user to load markerIdentity table of database.
                   uiOutput("markerTableChoice"),
                   dataTableOutput("colTable")),
          tabPanel("Experiment Classification",
                   # Allows user to load Classification table of database.
                   uiOutput("classTableChoice"),
                   dataTableOutput("classTable")),
          tabPanel("Equipment Information",
                   # Allows user to load equipmentInfo table of database.
                   uiOutput("equipTableChoice"),
                   dataTableOutput("equipTable")),
          tabPanel("User History",
                   # Loads user history table from database.
                   dataTableOutput("userTable"))))),

##################################################################################################
##################################################################################################
##################################################################################################
##### TAB: DELETE DATA #####
# Third tab contents.
tabItem(tabName = "delete",
      #### POP-UP: REQUESTING CONFIRMATION ####
      # Asks User if he sure of deleting the selected files.
      bsModal("sure", "Delete", trigger = "delete", size = "small",
              tags$head(tags$style("#sure .modal-footer{display:none}")),
              helpText("Are you sure you want to remove the selected files irrevocably?"),
              fluidRow(
                column(6, withBusyIndicatorUI(actionButton("yesSure", "Yes"))),
                column(6, actionButton("noSure", "No")))),
      # Explains what this tab is for.
      fluidRow(
        column(3,
               fluidRow(
                 br(), br(), br(),
                 tags$div(class="helpTextDelete",
                          helpText(HTML("<p align='justify' style='font-size:20px'>Here, you can <b>delete 
                                        specific files</b> from a database. You need to select the database you 
                                        want to edit. Subsequently, a project can be selected which enables the 
                                        possibility to choose contained files. Those will be displayed parallel 
                                        to the picking. Pressing the button 'Delete Files' opens a pop up window 
                                        where you can revise your input. 'Delete' will remove all traces of the 
                                        chosen files from the database - this step <b>cannot be revoked</b>. 
                                        'Redo' will delete the sample and the process can restart.</p>"))))),
        # Box with widgets to delete files from database.
        column(9,
               tags$div(class="boxDelete",
                        box(width = 11, height = 750, status = "info",
                            fluidRow(
                              column(6, br(),
                                     # Loads database. Later on with drop down.
                                     selectizeInput("deleteDBSelection", "Choose a Database:", 
                                                    choices = sub(".sqlite3", "", list.files(DB.path, pattern = "sqlite3$")),
                                                    options = list(placeholder = "", 
                                                                   onInitialize = I('function() {this.setValue(""); }'))),
                                     # Project to delete from.
                                     uiOutput("projectChoiceDelete"),
                                     br(), br(),
                                     # Possible files to delete.
                                     uiOutput("chooseFiles")),
                              
                              conditionalPanel(
                                condition = "output.chooseFiles",
                                column(6,
                                       br(),
                                       # Shows selected files which will be deleted.
                                       box(title = "View Selection", solidHeader = TRUE, status = "info", 
                                           collapsible = FALSE, width = 11,
                                           helpText("Those are the files that will be deleted:"),
                                           tableOutput("checkFiles")),
                                       br(), br(),
                                       # Button to delete files. Opens pop up to make sure first.
                                       actionButton("delete", "Delete Files"))))))))),

##################################################################################################
##################################################################################################
##################################################################################################
##### TAB: How to... #####
tabItem(tabName = "help",
      fluidRow(column(3, uiOutput("videoUpload")),
               column(3, uiOutput("videoPlot")),
               column(3, uiOutput("videoReview")),
               column(3, uiOutput("videoDelete")))),

##################################################################################################
##################################################################################################
##################################################################################################
##### TAB: IMPRESSUM #####
tabItem(tabName = "imp",
      fluidRow(
        column(5, uiOutput("Impressum"))))),


# Modal window opens if user hits 'Quit App'-button -> closes App and tab which disconnects
# connection with database and therefore saves all changes.
bsModal("quitDialog", "Quit Web App", "quit", size = "small",
    tags$head(tags$style("#quitDialog .modal-footer{display:none}")),
    helpText("Do you really want to quit?"),
    # App closes and disables Connection to database.
    actionButton("yes", "Yes"),
    # Modal window closes and nothing else.
    HTML("<button id='no' type='button' data-dismiss='modal' 
         class='btn'>No</button>"))
    ))))

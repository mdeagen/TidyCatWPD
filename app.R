library(shiny)

# TidyCatWPD: R Shiny Application for Tidying WebPlotDigitizer Data
# Author: M. Deagen (Michael.Deagen [at] uvm.edu)
# Date: 25 Feb. 2021

server <- function(input, output, session) {
    
    # When user clicks Download, assembles all tidy data into csv files that are assembled into a zip archive
    output$downloadData <- downloadHandler(
        
        # Name of tidy file
        filename = paste0("tidy",format(Sys.time(),"_%Y%m%d-%H%M%S"),".zip"),
        content = function(file) {
            fs <- c() # list of csv files to include in zip folder
            
            temp_dir <- tempdir() # go to temp directory
            setwd(temp_dir)
            
            to_tidy <- tidy_list()
            config_instance <- config_df()
            
            # iterate across uploaded csv's in tidy_list
            untidy_files <- gsub(pattern = "_tidy.csv", replacement = ".csv", to_tidy)
            
            for (i in 1:length(untidy_files)) {
                file_path <- input$file2$datapath[which(input$file2$name == untidy_files[i])]
                decode <- config_instance[which(config_instance$filename == untidy_files[i]),"series_decode"]
                
                tidied_df <- tidy(untidy_filename = file_path, series_decode = decode)
                
                # write csv (omit rows containing an NA value)
                write.csv(tidied_df[complete.cases(tidied_df),], file = to_tidy[i])
                fs <- c(fs, to_tidy[i])
            }
            zip(zipfile = file, files = fs)
        },
        contentType = "application/zip"
    )
    
    # Data frame that reads tidyconfig file and displays which files have been uploaded
    config_df <- reactive({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath)
                colnames(df) <- c("filename", "series_decode")
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        for(i in 1:nrow(df)){
            if(df$filename[i] %in% input$file2[,1]) df[i,"Uploaded?"] <- TRUE
            else df[i,"Uploaded?"] <- ""
        }
        return(df)
    })
    
    # Render overview table
    output$overview <- renderTable({
        return(config_df())
    })
    
    # On press of "Tidy!" run the R script that tidies all files and saves as data objects that can be previewed and then downloaded as a .zip file
    observeEvent(input$tidyButton,{
        req(input$file2)
        # Update dropdown menu options
        updateSelectInput(session, inputId = "convertedFiles", choices = tidy_list())
    })
    
    # Show a preview DataTable for the convertedFile that has been selected from the dropdown
    output$preview <- renderDataTable({
        # Do nothing if no valid files have been uploaded
        if(input$convertedFiles == "") return()
        
        config_instance <- config_df()
        file_to_preview <- gsub(pattern = "_tidy.csv", replacement = ".csv", input$convertedFiles)
        file_path <- input$file2$datapath[which(input$file2$name == file_to_preview)]
        decode <- config_instance[which(config_instance$filename == file_to_preview),"series_decode"]
        
        preview_df <- tidy(untidy_filename = file_path, series_decode = decode)
        
        return(preview_df)
    }, options = list(
        lengthMenu = list(c(5,15,-1),c('5','15','All')),
        pageLength = 5
    ))
    
    tidy_list <- reactive({
        tidy_list <- c()
        config_instance <- config_df()
        # iterate through config_df to determine valid files for tidying (those that have been uploaded, and are in tidyconfig.csv)
        for (i in 1:nrow(config_instance)){
            if(config_instance[i,"Uploaded?"]==TRUE){
                candidate_file <- gsub(pattern =".csv", replacement= "_tidy.csv",config_instance[i,"filename"])
                tidy_list <- append(tidy_list, candidate_file)
            } else next()
        }
        return(tidy_list)
    })
    
    #~~~HELPER FUNCTIONS~~~#
    
    # Function for extracting and splitting data series labels from first row of WPD file
    create_label_array <- function(csv_filename){
        con <- file(csv_filename,"r")
        first_line <- readLines(con,1)
        close(con)
        trim_comma <- substr(first_line,1,nchar(first_line)-1)
        label_list <- strsplit(trim_comma, split = ",,")
        label_array <- strsplit(label_list[[1]], split = "&")
        return(label_array)
    }
    
    # Function for transforming "untidy" WPD output into "tidy" format
    tidy <- function(untidy_filename, series_decode){
        # Read csv, skipping first line (labels)
        untidy_df <- read.csv(untidy_filename, skip = 1)
        # Initialize clean_df
        clean_df <- untidy_df
        
        # Parse encodings string from config_df
        encodings <- strsplit(series_decode,split = "&")
        # Append encodings to X and Y
        encodings_all <- append(c("X","Y"),encodings[[1]]) 
        
        # Create array of labels by parsing first line of untidy csv
        labels_df <- create_label_array(untidy_filename)
        
        # Iterate across all pairs of columns, adding extra encodings as headers and labels as values
        for (j in 1:length(labels_df)) { # stack pairs of columns into one tidy column
            tempdf <- untidy_df[,(2*j-1):(2*j)]
            colnames(tempdf) <- c("X","Y")
            for (k in 1:length(encodings[[1]])) {
                tempdf[, encodings[[1]][k]] <- labels_df[[j]][k]
            }
            if (j == 1){
                clean_df <- tempdf
            } else {
                clean_df <- rbind(clean_df, tempdf)
            }
        }
        return(clean_df)
    }
    
    
}

ui <- fluidPage(
    # Left panel (upload files)
    column(3,
           wellPanel(
               h1("TidyCatWPD"),
               em("App for Tidying WebPlotDigitizer Data"),
               br(),br(),
               a(href="TidyCatWPD_Tutorial.pdf","View Tutorial", target="_blank"),
               br(),br(),
               # File Input for tidyconfig.csv
               fileInput("file1", "1) Upload Configuration (e.g., tidyconfig.csv)",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               # File Input for csv's to Tidy (multiple allowed)
               fileInput("file2", "2) Upload WPD Data (e.g., fig1.csv, fig2.csv...)",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               actionButton("tidyButton","Tidy!", width = "100%", icon = icon("cat"), style = 'font-size:200%')
           )
    ),
    # Center panel (overview of config and which files have been uploaded)
    column(4,
           h3("Overview"),
           # Show overview of config and which files have been uploaded
           tableOutput("overview")
    ),
    # Right panel (conditional, preview datasets that have been tidied)
    column(5,
           conditionalPanel("input.tidyButton > 0",
                            wellPanel(
                                selectInput("convertedFiles","Preview Converted Files", choices = NULL, selectize = TRUE),
                                downloadButton("downloadData","Download Tidy Data"),
                                br(),br(),
                                dataTableOutput("preview")
                            )
           )
    )
)
# Run the application 
shinyApp(ui = ui, server = server)

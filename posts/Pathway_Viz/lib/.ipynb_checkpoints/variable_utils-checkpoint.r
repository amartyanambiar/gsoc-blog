# import os
# from IPython.display import Markdown

# def get_variable_from_link_or_input(variable, name = 'accession', default = None):
#     """
#     Get a variable value, either from an ENV VAR that would have been set by the shiny_proxy_jlab_query_parms extension, or through direct user input.
#     """
#     var = os.getenv(variable)
#     if var:
#         display(Markdown(f'<span style="background-color: #0a5032; color: #fff; padding: 8px;">Using {name} <emph>{var}</emph> from the link you followed.</span>'))
#     else:
#         var = input(f'Type {"an" if name[0].lower() in "aeiou" else "a"} {name} [default: {str(default)}]')
#     var = var or default
#     print(f'Using "{var}" as {name}')
#     return var

library(glue)

get_variable_from_link_or_input <- function(variable, name = 'accession', default = NA) {
    # Get a variable value, either from an ENV VAR that would have been set by the shiny_proxy_jlab_query_parms extension, or through direct user input.
    var <- Sys.getenv(variable, unset = NA)
    if (!is.na(var)) {
        print(glue('Using {name} = {var} from the link you followed.'))
    } else {
        determiner <- ifelse(grepl(tolower(substr(name, 0, 1)), 'aeiou'), 'an', 'a')
        var <- readline(prompt = glue("Type {determiner} {name} [default: {default}]"))
    }
    var <- ifelse(is.na(var) || var == '', default, var)
    var
}

get_pathway_info <- function(pathway) {
  pathway <- paste("map", pathway, sep = "")
  pathway_name <- keggList(pathway)[[1]]
  pathway_url <- paste("https://www.kegg.jp/pathway/", pathway, sep = "")
  return(list(pathway_name = pathway_name, pathway_url = pathway_url))
}

PathwaysSelection <- function() {
  cat("Pathways Selection :\n",
      "- For the most general & most complete pathways, input 'G'\n",
      "- Press Enter to generate the most complete pathways\n",
      "- To add custom pathways, input pathway numbers (ex: 00053,01220)\n\n")
    
  flush.console()
  CUSTOM_PATHWAY_IDS <- get_variable_from_link_or_input('CUSTOM_PATHWAY_IDS', name = 'Pathways Accession', default = '')
  
  if (CUSTOM_PATHWAY_IDS == "") {
    CUSTOM_PATHWAY_IDS <- list()
  } else if (CUSTOM_PATHWAY_IDS == "G") {
    CUSTOM_PATHWAY_IDS <- list("00010", "00020", "00030", "00061", "01232","00230","00240", "00190")
  } else {
    CUSTOM_PATHWAY_IDS <- strsplit(CUSTOM_PATHWAY_IDS, ",")[[1]]
  }
  
  message(if (length(CUSTOM_PATHWAY_IDS) > 0) {
    paste("\nUsing", CUSTOM_PATHWAY_IDS, " - ", sapply(CUSTOM_PATHWAY_IDS, function(id) paste(get_pathway_info(id)[1]," : ",get_pathway_info(id)[2])), "as a Custom Pathway")
  } else {
    "\nUsing NONE as a Custom Pathway"
  })
    return(CUSTOM_PATHWAY_IDS)
}


generatePathwayPlots <- function() {
# Clearing the current working directory
  if (!dir.exists("pathway_plots")) {
    dir.create("pathway_plots")
  }

  file.copy(from = list.files(pattern = "./*pathview.png"), to = "./pathway_plots/", overwrite = TRUE)

  png_files <- list.files(path = ".", pattern = "*.png")
  xml_files <- list.files(path = ".", pattern = "*.xml")
  files <- c(png_files, xml_files)
  output <- capture.output({
    unlink(files)
  })
  
# Accessing the png files and displaying it
  images <- list.files("pathway_plots", full.names = TRUE)

  for (pathway in images) {
    print(get_pathway_info(gsub("[^0-9]", "", basename(pathway)))$pathway_name)
    Sys.sleep(1)
    flush.console()
    display_png(file = pathway)
  }
}
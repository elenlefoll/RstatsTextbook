extract_sections <- function(input_file) {
  if (!file.exists(input_file)) {
    stop("Input file does not exist: ", input_file)
  }
  
  lines <- readLines(input_file)
  extracted_lines <- character()
  in_oer_block <- FALSE
  current_colons <- 0
  block_start <- NULL
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Check for OER block opening
    if (grepl("^:::{1,6}\\s*\\{\\.content-visible\\s+when-profile=\"OER\"\\}", line)) {
      # Count exact colons in opening line
      current_colons <- length(gregexpr(":", line)[[1]])
      in_oer_block <- TRUE
      block_start <- i
      next  # Skip opening line
    }
    
    # Check for block closing (must exactly match current_colons)
    if (in_oer_block) {
      # Create exact pattern for closing marker
      closing_pattern <- paste0("^", strrep(":", current_colons), "\\s*$")
      if (grepl(closing_pattern, line)) {
        in_oer_block <- FALSE
        cat("Extracted OER block from line", block_start, "to", i, "\n")
        next  # Skip closing line
      }
    }
    
    # If we're in an OER block, keep the content
    if (in_oer_block) {
      extracted_lines <- c(extracted_lines, line)
    }
  }
  
  # Only warn if we're actually in a block at the end
  if (in_oer_block) {
    warning("OER block starting at line ", block_start, " not closed in file: ", input_file)
  }
  
  return(extracted_lines)
}

# Main function to process all files
process_directory <- function() {
  # List all .qmd files
  files <- dir(pattern = "*.qmd$")

  # Filter out files that end with _quiz.qmd so as to overwrite any existing _quiz.qmd files
  files <- files[!grepl("_quiz\\.qmd$", files)]  
  
  # Filter files starting with one or two digits followed by an underscore
  files <- files[grepl("^\\d{1,2}_", files)]
  
  # Process each file
  for (file in files) {
    tryCatch(
      expr = {
        # Extract sections
        extracted_lines <- extract_sections(file)
        
        # Extract chapter number from filename
        chapter_number <- gsub("\\D", "", file)
        
        # Create output file name with quizzes directory
        base_name <- tools::file_path_sans_ext(file)
        output_file <- file.path("quizzes", paste0(chapter_number, "_quiz.qmd"))
        
        # Create quizzes directory if it doesn't exist
        dir.create("quizzes", showWarnings = FALSE)
        
        # Check if output file already exists
        if (file.exists(output_file)) {
          file.remove(output_file)
          cat("Existing output file removed:", output_file, "\n")
        }
        
        # Write extracted content to new file
        writeLines(extracted_lines, output_file)
        
        # Add header to the new file
        header <- c(
          paste("# Ch. ", chapter_number, ": Tasks & Quizzes {.unnumbered}", sep=""),
          ""
        )
        
        # Insert header at the beginning
        final_content <- c(header, extracted_lines)
        writeLines(final_content, output_file)
        
        cat("Successfully processed Ch.", chapter_number, ":", file, "\n")
      },
      error = function(e) {
        cat("Error processing:", file, ":", e$message, "\n")
      }
    )
  }
}  
  
# Run the processing function
process_directory()


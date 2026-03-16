extract_sections <- function(input_file) {
  if (!file.exists(input_file)) {
    stop("Input file does not exist: ", input_file)
  }
  
  lines <- readLines(input_file)
  in_target_block <- FALSE
  nesting_level <- 0
  extracted_lines <- character()
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Match opening block: :::: {.content-visible when-profile="OER"}
    # Allow optional whitespace around the opening
    if (grepl("^::::\\s*\\{\\.content-visible\\s+when-profile=\"OER\"\\}", line)) {
      nesting_level <- nesting_level + 1
      in_target_block <- TRUE
      cat("Entered target block at line", i, "\n")
      next  # Skip adding this line
    }
    
    # Match closing block: ::::
    # Only count it if it's a standalone closing block (no content inside)
    # This avoids matching other `::::` lines (e.g., in code chunks or other blocks)
    if (grepl("^::::\\s*$", line)) {
      if (nesting_level > 0) {
        nesting_level <- nesting_level - 1
        if (nesting_level == 0) {
          in_target_block <- FALSE
          cat("Exited target block at line", i, "\n")
        }
      }
      next  # Skip adding this line
    }
    
    # Add line only if we're inside the target block and not a block delimiter
    if (in_target_block) {
      extracted_lines <- c(extracted_lines, line)
    }
  }
  
  # Optional: Warn if block wasn't closed
  if (nesting_level > 0) {
    warning("Block not closed in file: ", input_file)
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
        
        # Create output file name
        base_name <- tools::file_path_sans_ext(file)
        output_file <- paste0(chapter_number, "_quiz.qmd")
        
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
          "---",
          "")
        
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

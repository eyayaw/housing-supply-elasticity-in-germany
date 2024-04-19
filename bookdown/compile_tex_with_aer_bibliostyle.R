# Why this function? [GitHub link](https://github.com/rstudio/bookdown/issues/1403)

# When `biblio-style: "aer"` and `citation_package: "natbib"`, bookdown places `References` after `Appendix`.
# A solution involves manipulating the tex file to rearrange parts and then rendering it using `tinytex::xelatex`.

## Steps
# 1. Update the bibliography style.
# 2. Render the project.
# 3. Relocate the bibliography.
# 4. Compile the tex file.

# gets the filename from the _bookdown.yml file
get_filename = function(yml_path = "_bookdown.yml") {
  stopifnot("File does not exist" = file.exists(yml_path))
  bookdown_yml = yaml::yaml.load_file(yml_path)

  d_exists  = !is.null(bookdown_yml$output_dir)
  fname_exists = !is.null(bookdown_yml$book_filename)

  if (d_exists && fname_exists) {
    return(with(bookdown_yml, file.path(output_dir, book_filename)))
  } else if (fname_exists) {
    return(bookdown_yml$book_filename)
  } else {
    stop("We can't find the book filename.", call. = F)
  }
}


backup_file = function(from, to = NULL, rm_original = F) {
  stopifnot("File does not exist. Backup can't be made." = file.exists(from))
  if (is.null(to)) {
    to = tempfile(fileext = paste0(".", tools::file_ext(from)))
  }
  file.copy(from, to, overwrite = T)
  if (rm_original) {
    unlink(from)
  }
  message(sprintf("`%s` backed up to `%s`.\n", from, to))
  invisible(to)
}


render_with_aer_biblio_style <- function(input_file = "index.Rmd") {
  original_content = readLines(input_file)

  # create safe original copies
  files = c(input_file, paste0(basename(get_filename()), ".Rmd"), paste0(get_filename(), ".pdf"))
  backups = vapply(files, backup_file, "")
  on.exit(Map(\(f, t) backup_file(f, t, T), backups, files), add = T)

  # locate the yaml section
  yaml_delims = grep("^---$", original_content)

  if (length(yaml_delims) < 2) {
    stop("We can't find the yaml header.", call. = F)
  }

  if (length(yaml_delims) > 2) {
    warning("The length of yaml_delimiters is > 2, keeping only the first two.", call. = F)
    yaml_delims = yaml_delims[1:2]
  }

  metadata = original_content[yaml_delims[1]:yaml_delims[2]]
  metadata_yaml = yaml::yaml.load(metadata)

  # update yaml with new biblio-style (here aer) and citation package
  metadata_yaml$csl = NULL
  metadata_yaml$`biblio-style` = "aer"
  output_format = names(metadata_yaml$output)[1] # consider the 1st format
  metadata_yaml$output[[output_format]]$citation_package = "natbib"

  # Re-construct metadata
  updated_metadata_yaml = yaml::as.yaml(metadata_yaml)
  updated_metadata_yaml = paste0("---\n", updated_metadata_yaml, "---\n")
  updated_content = c(updated_metadata_yaml, original_content[-(yaml_delims[1]:yaml_delims[2])])

  writeLines(updated_content, input_file)

  ## try to render site, catch any errors

  # `render_site`'s purpose below is to generate the new .tex file,
  # it will be compiled to create a new pdf but we do not need it.
  # That is why we revert to the original above with the backups.
  # Since the references will be after the appendix in the new pdf anyway, as noted at the start of this document.

  tryCatch(
    rmarkdown::render_site(output_format = output_format, encoding = "UTF-8"),
    error = function(e) message("Some error has occured.\n", e)
  )
}


# move the bibliography to the desired location in the file (i.e. before the Appendix)
relocate_biblio = function(tex_path) {
  if (!file.exists(tex_path)) {
    stop(paste("File not found:", tex_path), call. = F)
  }

  # load the tex file
  input = readLines(tex_path)

  # locate key sections
  bibstyle_loc = grep("\\bibliographystyle{", input, fixed = TRUE)
  nocite_loc = grep("\\nocite", input, fixed = TRUE)
  biblio_loc = grep("\\bibliography{", input, fixed = TRUE)
  ref_loc = grep("{References}\\label{references}", input, fixed = TRUE)

  if (length(ref_loc) == 0 || length(c(bibstyle_loc, nocite_loc, biblio_loc)) == 0) {
    stop("Bibliography or reference section not found.", call. = F)
  }

  # relocate bibliography
  input[ref_loc] = paste0(input[c(bibstyle_loc, nocite_loc, biblio_loc)], collapse = "\n")

  write(input[-c(bibstyle_loc, nocite_loc, biblio_loc)], tex_path)

  if (Sys.getenv("RSTUDIO") != "") {
    invisible(rstudioapi::navigateToFile(tex_path))
  }

  invisible(tex_path)
}


## action: render + compile
render_with_aer_biblio_style()
tex_path = paste0(get_filename(), ".tex")
relocate_biblio(tex_path)
if (!interactive())
  print("Compiling ... hang on a bit, it may take a while.")
tinytex::xelatex(
  tex_path, pdf_file = paste0(tools::file_path_sans_ext(tex_path), "_aer.pdf")
)

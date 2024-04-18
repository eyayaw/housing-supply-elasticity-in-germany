
get_tbl_body = function(file_path, tag = "table", rm_caption = TRUE, rm_label = TRUE) {
  latex_code = readLines(file_path)
  tag_start = grep(paste0("\\\\begin\\{", tag, "\\}"), latex_code, perl = TRUE)
  tag_end = grep(paste0("\\\\end\\{", tag, "\\}"), latex_code)

  if (length(tag_start) == 0 || length(tag_end) == 0) {
    return(NULL)
  }
  lines = latex_code[(tag_start + 1):(tag_end - 1)]

  # remove caption and label
  if (rm_caption)
    lines = lines[!grepl("\\\\caption\\{[^\\}]*\\}", lines)]
  if (rm_label)
    lines = lines[!grepl("\\\\label\\{[^\\}]*\\}", lines)]
  return(lines)
}

get_tbl_caption = function(file_path) {
  latex_code = readLines(file_path)
  # Parse caption
  caption = latex_code[grep("\\\\caption\\{[^\\}]*\\}", latex_code)]
  caption = gsub("\\\\caption\\{(.*)\\}", "\\1", caption)
  return(caption)
}
get_tbl_label = function(file_path) {
  latex_code = readLines(file_path)
  # Parse label
  label = latex_code[grep("\\\\label\\{[^\\}]*\\}", latex_code)]
  label = gsub("\\\\label\\{(.*)\\}", "\\1", label)
  label = gsub("(tab|tbl)[:-]", "", label)
  return(label)
}


create_tbl_chunk = function(file_path, ..., caption = NULL, label = NULL) {
  lines = get_tbl_body(file_path)
  if (is.null(lines)) {
    return(NULL)
  }
  if (is.null(caption)) {
    caption = get_tbl_caption(file_path)
  }
  if (is.null(label)) {
    label = get_tbl_label(file_path)
  }
  add_opts = paste0(..., collapse = "\n")
  raw_chunk = paste0(
    "```{r}\n",
    "#| label: tbl-", label, "\n",
    "#| tbl-cap: '", caption, "'\n",
    "#| output: asis\n",
    add_opts,
    "\n",
    "source('output/create_tbl_chunk.R')\n\n",
    "lines = get_tbl_body('", file_path, "', rm_caption=TRUE, rm_label=TRUE)\n",
    "cat(lines)",
    "\n",
    "```"
  )
  return(raw_chunk)
}

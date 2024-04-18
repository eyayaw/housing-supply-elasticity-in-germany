source("./output/create_tbl_chunk.R")

tex_files = dir("output/", pattern = "\\.tex$", full.names = TRUE)

for (file_path in tex_files) {
  chunk = create_tbl_chunk(file_path)
  if (!is.null(chunk)) {
    cat(chunk, file = paste0(tools::file_path_sans_ext(file_path), ".qmd"), append = FALSE)
  }
}

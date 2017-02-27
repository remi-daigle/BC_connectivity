# load library
require(rmarkdown)

#### render readme ####
render("R/README.Rmd", "pdf_document",output_dir="..")

# # cleanup
# unlink("R/README.utf8.md")
# file.rename("R/README.knit.md","README.md")

#### render manuscript ####
render("doc/manuscript.Rmd", "pdf_document")
render("doc/manuscript.Rmd", "word_document")
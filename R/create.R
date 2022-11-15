#' Create a new postcard
#'
#' Create (and optionally edit) a new postcard.
#'
#' @inheritParams rmarkdown::draft
#' @param create_dir `TRUE` to create a new directory for the postcard
#'   (defaults to `FALSE`).
#' @param create_image `TRUE` to create a placeholder image
#'   (defaults to `TRUE`).
#'
#' @export
#' @details
#' Valid values for the `template` argument include `"jolla"`, `"jolla-blue"`,
#' `"trestles"`, `"trestles-mod"`, `"onofre"`, `"solana-mod"`, and `"solana"`.
#' @examples
#' \dontrun{
#'
#' postcards::create_postcard(template = "jolla")
#' postcards::create_postcard(template = "jolla-blue")
#' postcards::create_postcard(template = "trestles")
#' postcards::create_postcard(template = "trestles-mod")
#' postcards::create_postcard(template = "onofre")
#' postcards::create_postcard(template = "solana")
#' #' postcards::create_postcard(template = "solana-mod")
#' }
create_postcard <- function(file = "index.Rmd",
                            template = "jolla",
                            create_dir = FALSE,
                            edit = TRUE,
                            create_image = TRUE) {

  article <- rmarkdown::draft(
    file,
    template,
    "postcards",
    create_dir = create_dir,
    edit = FALSE
  )

  if(create_image) {
    img_table <- as.list(
      system.file("img",
                  c("tobi.jpg", "xiang.jpg", "frank.jpg", "herzl.jpg", "sigridur.jpg"),
                  package = "postcards"))

    names(img_table) <- c("jolla", "jolla-blue", "trestles", "trestles-mod", "onofre", "solana", "solana-mod")
    file.copy(img_table[[template]], dirname(file))
  }

  if (edit) {
    if (rstudioapi::hasFun("navigateToFile"))
      rstudioapi::navigateToFile(file)
    else
      utils::file.edit(file)
  }

  invisible(article)
}

new_project_create_postcard <- function(path, ...) {
  params <- list(...)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (rstudioapi::isAvailable("1.1.287")) {
    rstudioapi::initializeProject(path)
  }

  template_table <- as.list(c("jolla", "jolla-blue", "trestles", "trestles-mod", "onofre", "solana", "solana-mod"))
  names(template_table) <- c("Jolla", "Jolla Blue", "Trestles", "Trestles-mod", "Onofre", "Solana", "Solana-mod")

  create_postcard(file.path(path, "index.Rmd"),
                  template = template_table[[params[["template"]]]],
                  edit = FALSE)
}



get_teacher_info <- function(dir) {
  teacher_name <- str_split(dir, "/", simplify = TRUE)[, 3] |>
    str_replace_all("_", " ") |>
    str_to_title()

  teacher_files <- list.files(dir, full.names = TRUE)

  teacher_desc <- teacher_files[grep("desc_", teacher_files)]
  teacher_photo <- teacher_files[grep("foto", teacher_files)]
  teacher_linkedin <- teacher_files[grep("linkedin", teacher_files)]

  if (str_detect(teacher_name, "Angeles Varo")) {
    teacher_name <- "Mª Ángeles Varo"
  }

  list(
    name  = teacher_name,
    desc  = teacher_desc,
    photo = teacher_photo,
    linkedin = teacher_linkedin
  )
}



teacher_card <- function(name, photo.dir, desc.dir, linkedin) {
  div(
    class = "teacher",
    div(
      class = "teacher__img-container",
      tags$img(
        src   = photo.dir,
        alt   = "Imagen",
        class = "teacher__img"
      )
    ),
    tags$h5(
      name,
      class = "teacher__name"
    ),
    tags$div(
      desc.dir,
      class = "teacher__desc"
    ),
    tags$button(
      class = "btn teacher__btn",
      tags$a(
        class = "teacher__btn-text",
        tags$i(class = "fa-brands fa-linkedin", style = "margin-right: 8px;"),
        "Linkedin",
        href   = linkedin,
        style  = "text-decoration: none;",
        target = "_blank"
      )
    )
  )
}



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


agenda_table <- function(sheet, subtitle = "*Día 1: Herramientas geoinformáticas*") {
  dat <- read_xlsx("00_assets/data/BD-jornadas-202506.xlsx", sheet = sheet) |>
    mutate(
      Comienzo = as.character(Comienzo) |> str_sub(start = 12, end = 16),
      Fin      = as.character(Fin) |> str_sub(start = 12, end = 16)
    ) |>
    fill(
      Comienzo:Fin,
      .direction = "down"
    ) |>
    mutate(Horario  = paste(Comienzo, "-", Fin)) |>
    select(Horario, Título:Coordinador) |>
    group_by(Horario) |>
    mutate(id = cur_group_id()) |>
    ungroup()


  dat |>
    select(-id) |>
    gt(
      groupname_col       = "Horario",
      row_group_as_column = TRUE
    ) |>
    tab_header(
      title = "Agenda",
      subtitle = md(subtitle)
    ) |>
    tab_options(
      # Alineación y fuente
      heading.align = "center",
      table.font.size = px(14),
      data_row.padding = px(5),
      row_group.padding = px(10),
      # Bordes de la tabla
      table.border.top.style = "solid",
      table.border.top.width = px(2),
      table.border.top.color = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(2),
      table.border.bottom.color = "black",
      # Bordes en los grupos de filas
      row_group.border.top.style = "solid",
      row_group.border.top.width = px(1),
      row_group.border.top.color = "gray",
      row_group.border.bottom.style = "solid",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "gray"
    ) |>
    ## title style
    tab_style(
      style = list(
        cell_fill(color = "#002500"),
        cell_text(color = "#e1e3e1", weight = "bold")
      ),
      locations = cells_title(groups = c("title", "subtitle"))
    ) |>
    ## horario column (even rows)
    tab_style(
      style = list(
        cell_text(color = "#FFCF99", weight = "bold"),  # Peach title text
        cell_fill(color = "#8C6239")  # Brown background
      ),
      locations = cells_row_groups()
    ) |>
    ## body rows (even rows)
    tab_style(
      style = list(
        cell_fill(color = "#f9ffe9"),  # Peach title text
        cell_text(color = "#002500")  # Brown background
      ),
      locations = cells_body(
        rows = which(dat$id %% 2 == 0)
      )
    ) |>
    ## body rows (uneven rows)
    tab_style(
      style = list(
        cell_fill(color = "#e1e3e1"),  # Peach title text
        cell_text(color = "#002500")  # Brown background
      ),
      locations = cells_body(
        rows = which(dat$id %% 2 != 0)
      )
    ) |>
    opt_table_font("Merriweather") |>
    ## dencansos
    tab_style(
      style     = list(
        cell_fill(color = "#002500"),
        cell_text(color = "#e1e3e1")
      ),
      locations = cells_body(rows = which(dat$Tipo == "Descanso"))
    ) |>
    ## column titles
    tab_style(
      style     = list(
        cell_text(color = "#FFCF99", weight = "bold"),  # Peach title text
        cell_fill(color = "#8C6239")  # Brown background
      ),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style     = list(
        cell_fill(color = "#002500"),
        cell_text(color = "#e1e3e1")
      ),
      locations = cells_title()
    ) |>
    cols_width(
      Horario ~ px(110)
    ) |>
    ## horario column title
    tab_stubhead("Horario") |>
    tab_style(
      style = list(
        cell_fill(color = "#8C6239"),
        cell_text(color = "#FFCF99", weight = "bold")
      ),
      locations = cells_stubhead()
    ) |>
    ## missing values
    fmt_missing(missing_text = "")
}

agenda_table(3) |>
  gtsave("00_assets/figures/agenda-d1.png")

agenda_table(4, "*Dia 2: Teledetección e Inteligencia Artificial*") |>
  gtsave("00_assets/figures/agenda-d2.png")

agenda_table(5, "*Dia 3: Teledetección próxima a la Tierra*") |>
  gtsave("00_assets/figures/agenda-d3.png")

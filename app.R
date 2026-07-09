library(shiny)

capitalize_first <- function(x) {
    sub("^([^A-Za-z]*)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

read_prompts <- function(path = "prompts.txt") {
    if (!file.exists(path)) {
        stop("Could not find '", path, "'. Make sure it is in the app's working directory.")
    }
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    lines <- sub("^-\\s*", "", lines)
    lines <- capitalize_first(lines)
    lines
}

generate_board <- function(all_prompts, w, h, want_free) {
    n_cells <- w * h
    use_free <- isTRUE(want_free) && (w %% 2 == 1) && (h %% 2 == 1)

    free_text <- "FREE"
    prompts_pool <- all_prompts
    if (use_free) {
        match_idx <- which(tolower(prompts_pool) == "session goes over time")
        if (length(match_idx) > 0) {
            free_text <- prompts_pool[match_idx[1]]
            prompts_pool <- prompts_pool[-match_idx[1]]
        }
    }

    n_needed <- if (use_free) n_cells - 1 else n_cells
    insufficient <- length(prompts_pool) < n_needed

    if (!insufficient) {
        chosen <- sample(prompts_pool, n_needed, replace = FALSE)
    } else {
        chosen <- sample(prompts_pool, n_needed, replace = TRUE)
    }

    cells <- vector("character", n_cells)
    center_idx <- NA
    if (use_free) {
        center_idx <- ceiling(n_cells / 2)
        cells[-center_idx] <- chosen
        cells[center_idx] <- free_text
    } else {
        cells <- chosen
    }

    list(w = w, h = h, cells = cells, center_idx = center_idx, insufficient = insufficient)
}

fit_cell_text <- function(text, base_fontsize, min_fontsize = 7) {
    fontsize <- base_fontsize
    wrapped <- text
    repeat {
        full_width_npc <- grid::convertWidth(
            grid::grobWidth(grid::textGrob(text, gp = grid::gpar(fontsize = fontsize))),
            "npc", valueOnly = TRUE
        )
        avg_char_npc <- full_width_npc / max(1, nchar(text))
        chars_per_line <- max(3, floor(0.86 / avg_char_npc))

        wrapped <- paste(strwrap(text, width = chars_per_line), collapse = "\n")

        width_npc <- grid::convertWidth(
            grid::grobWidth(grid::textGrob(wrapped, gp = grid::gpar(fontsize = fontsize))),
            "npc", valueOnly = TRUE
        )
        height_npc <- grid::convertHeight(
            grid::grobHeight(grid::textGrob(wrapped, gp = grid::gpar(fontsize = fontsize))),
            "npc", valueOnly = TRUE
        )

        if ((width_npc <= 0.88 && height_npc <= 0.86) || fontsize <= min_fontsize) {
            break
        }
        fontsize <- fontsize - 1
    }
    list(text = wrapped, fontsize = fontsize)
}

draw_bingo_grid <- function(bd, title = NULL, font_size = 14) {
    w <- bd$w
    h <- bd$h
    cells <- bd$cells
    center_idx <- bd$center_idx
    free_font_size <- font_size + 4

    grid::grid.newpage()

    if (!is.null(title)) {
        grid::pushViewport(grid::viewport(y = grid::unit(1, "npc"), height = grid::unit(0.9, "inches"), just = "top"))
        grid::grid.text(title, gp = grid::gpar(fontsize = 22, fontface = "bold"))
        grid::upViewport()
        board_vp <- grid::viewport(
            y = 0, height = grid::unit(1, "npc") - grid::unit(0.9, "inches"),
            just = "bottom", layout = grid::grid.layout(h, w)
        )
    } else {
        board_vp <- grid::viewport(layout = grid::grid.layout(h, w))
    }

    grid::pushViewport(board_vp)

    for (i in seq_along(cells)) {
        row <- ceiling(i / w)
        col <- i - (row - 1) * w
        is_free <- !is.na(center_idx) && i == center_idx

        grid::pushViewport(grid::viewport(
            layout.pos.row = row, layout.pos.col = col, clip = "on"
        ))

        grid::grid.rect(
            gp = grid::gpar(
                fill = if (is_free) "#ffe58a" else "#fafafa",
                col = "#333333",
                lwd = 2
            ),
            width = grid::unit(0.96, "npc"),
            height = grid::unit(0.96, "npc")
        )

        fit <- fit_cell_text(cells[i], base_fontsize = if (is_free) free_font_size else font_size)

        grid::grid.text(
            fit$text,
            gp = grid::gpar(
                fontsize = fit$fontsize,
                fontface = if (is_free) "bold" else "plain"
            )
        )

        grid::upViewport()
    }

    grid::upViewport()
}

ui <- fluidPage(
    titlePanel("Conference Bingo"),

    tabsetPanel(
        tabPanel(
            "Bingo Board",
            fluidRow(
                column(
                    width = 2,
                    numericInput("size", "Board size:", value = 5, min = 1, max = 10),
                    checkboxInput("free_space", "Free space (odd sizes only)", value = FALSE),
                    actionButton("generate", "Generate new board", class = "btn-primary"),
                    br(), br(),
                    downloadButton("download_png", "Download as PNG")
                ),
                column(
                    width = 10,
                    uiOutput("bingo_board")
                )
            )
        ),

        tabPanel(
            "Multi-board PDF",
            fluidRow(
                column(
                    width = 2,
                    numericInput("id_size", "Board size:", value = 5, min = 1, max = 10),
                    checkboxInput("id_free_space", "Free space (odd sizes only)", value = FALSE),
                    sliderInput("id_font_size", "Text size:", min = 8, max = 24, value = 14, step = 1),
                    numericInput("num_boards", "Number of boards:", value = 30, min = 1, max = 500),
                    downloadButton("download_pdf", "Download PDF", class = "btn-primary")
                ),
                column(
                    width = 10,
                    tags$ul(
                        style = "font-size: 22px; margin-top: 40px;",
                        tags$li("Generates multiple boards at once, then creates a PDF with each board on a separate page."),
                        tags$li("The width/height and free space settings are the same for each board."),
                        tags$li("Board IDs are shown per-board.")
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {

    prompts <- reactive({
        read_prompts("prompts.txt")
    })

    board_data <- eventReactive(input$generate, {
        w <- input$size
        h <- input$size
        all_prompts <- prompts()

        if (length(all_prompts) == 0) {
            stop("prompts.txt has no usable sentences.")
        }

        bd <- generate_board(all_prompts, w, h, input$free_space)

        if (isTRUE(bd$insufficient)) {
            showNotification(
                "Not enough unique sentences for this board size — some will repeat.",
                type = "warning", duration = 6
            )
        }

        bd
    }, ignoreNULL = FALSE)

    output$bingo_board <- renderUI({
        bd <- tryCatch(board_data(), error = function(e) e)

        if (inherits(bd, "error")) {
            return(div(class = "alert alert-danger", conditionMessage(bd)))
        }

        w <- bd$w
        h <- bd$h
        cells <- bd$cells
        center_idx <- bd$center_idx

        cell_tags <- lapply(seq_along(cells), function(i) {
            is_free <- !is.na(center_idx) && i == center_idx
            div(
                class = if (is_free) "bingo-cell bingo-free" else "bingo-cell",
                cells[i]
            )
        })

        tagList(
            tags$style(HTML(sprintf("
        .bingo-scroll {
          overflow: auto;
          max-width: 100%%;
        }
        .bingo-grid {
          display: grid;
          grid-template-columns: repeat(%d, 190px);
          grid-template-rows: repeat(%d, 190px);
          gap: 6px;
          width: max-content;
        }
        .bingo-cell {
          border: 2px solid #333;
          border-radius: 6px;
          padding: 14px;
          box-sizing: border-box;
          width: 190px;
          height: 190px;
          display: flex;
          align-items: center;
          justify-content: center;
          text-align: center;
          font-size: 20px;
          line-height: 1.25;
          background-color: #fafafa;
        }
        .bingo-free {
          background-color: #ffe58a;
          font-weight: bold;
          font-size: 24px;
        }
      ", w, h))),
      div(class = "bingo-scroll", div(class = "bingo-grid", cell_tags))
        )
    })

    outputOptions(output, "bingo_board", suspendWhenHidden = FALSE)

    output$download_png <- downloadHandler(
        filename = function() {
            paste0("bingo_board_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
        },
        content = function(file) {
            bd <- board_data()
            cell_px <- 260
            grDevices::png(file, width = bd$w * cell_px, height = bd$h * cell_px, res = 150)
            on.exit(grDevices::dev.off(), add = TRUE)
            draw_bingo_grid(bd)
        },
        contentType = "image/png"
    )

    output$download_pdf <- downloadHandler(
        filename = function() {
            paste0("bingo_boards_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
        },
        content = function(file) {
            all_prompts <- prompts()
            w <- input$id_size
            h <- input$id_size
            n <- input$num_boards

            if (length(all_prompts) == 0) {
                stop("prompts.txt has no usable sentences.")
            }
            if (is.na(n) || n < 1) {
                stop("Enter a number of boards of at least 1.")
            }

            page_width <- w * 2.3
            page_height <- h * 2.3 + 1

            grDevices::pdf(file, width = page_width, height = page_height)
            on.exit(grDevices::dev.off(), add = TRUE)

            warned <- FALSE
            for (id in seq_len(n)) {
                bd <- generate_board(all_prompts, w, h, input$id_free_space)
                if (isTRUE(bd$insufficient) && !warned) {
                    showNotification(
                        "Not enough unique sentences for this board size — some will repeat across boards.",
                        type = "warning", duration = 6
                    )
                    warned <- TRUE
                }
                draw_bingo_grid(bd, title = paste0("Board ID: ", id), font_size = input$id_font_size)
            }
        },
        contentType = "application/pdf"
    )
}

shinyApp(ui = ui, server = server)

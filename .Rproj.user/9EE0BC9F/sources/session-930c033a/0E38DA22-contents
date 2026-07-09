library(shiny)

# ---- Helper: capitalize the first letter found in a string, leaving any ----
# ---- leading punctuation/quotes before it untouched ----
capitalize_first <- function(x) {
    sub("^([^A-Za-z]*)([a-z])", "\\1\\U\\2", x, perl = TRUE)
}

# ---- Helper: read prompts.txt and clean up the leading "- " from each line ----
read_prompts <- function(path = "prompts.txt") {
    if (!file.exists(path)) {
        stop("Could not find '", path, "'. Make sure it is in the app's working directory.")
    }
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]                 # drop blank lines
    lines <- sub("^-\\s*", "", lines)              # strip leading "- "
    lines <- capitalize_first(lines)               # capitalize first letter
    lines
}

ui <- fluidPage(
    titlePanel("Conference Bingo"),

    fluidRow(
        column(
            width = 2,
            numericInput("size", "Board size:", value = 5, min = 1, max = 10),
            checkboxInput("free_space", "FREE center space (odd sizes only)", value = FALSE),
            actionButton("generate", "Generate new board", class = "btn-primary"),
            br(), br(),
            downloadButton("download_png", "Download as PNG")
        ),

        column(
            width = 10,
            uiOutput("bingo_board")
        )
    )
)

server <- function(input, output, session) {

    prompts <- reactive({
        read_prompts("prompts.txt")
    })

    # Reactive value holding the current board's sentence assignment
    board_data <- eventReactive(input$generate, {
        w <- input$size
        h <- input$size
        n_cells <- w * h

        all_prompts <- prompts()

        use_free <- isTRUE(input$free_space) && (w %% 2 == 1) && (h %% 2 == 1)
        n_needed <- if (use_free) n_cells - 1 else n_cells

        if (length(all_prompts) == 0) {
            stop("prompts.txt has no usable sentences.")
        }

        if (length(all_prompts) >= n_needed) {
            chosen <- sample(all_prompts, n_needed, replace = FALSE)
        } else {
            # Not enough unique sentences: sample with replacement and warn
            showNotification(
                sprintf("Only %d sentences available but %d needed — some will repeat.",
                        length(all_prompts), n_needed),
                type = "warning", duration = 6
            )
            chosen <- sample(all_prompts, n_needed, replace = TRUE)
        }

        cells <- vector("character", n_cells)
        center_idx <- NA
        if (use_free) {
            center_idx <- ceiling(n_cells / 2)
            cells[-center_idx] <- chosen
            cells[center_idx] <- "FREE"
        } else {
            cells <- chosen
        }

        list(w = w, h = h, cells = cells, center_idx = center_idx)
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
        .bingo-grid {
          display: grid;
          grid-template-columns: repeat(%d, 1fr);
          grid-template-rows: repeat(%d, 1fr);
          gap: 6px;
          max-width: 1100px;
        }
        .bingo-cell {
          border: 2px solid #333;
          border-radius: 6px;
          padding: 14px;
          min-height: 150px;
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
      div(class = "bingo-grid", cell_tags)
        )
    })

    # Auto-generate a board when the app first loads
    outputOptions(output, "bingo_board", suspendWhenHidden = FALSE)

    output$download_png <- downloadHandler(
        filename = function() {
            paste0("bingo_board_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
        },
        content = function(file) {
            bd <- board_data()
            w <- bd$w
            h <- bd$h
            cells <- bd$cells
            center_idx <- bd$center_idx

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

            cell_px <- 260
            png(file, width = w * cell_px, height = h * cell_px, res = 150)
            on.exit(dev.off(), add = TRUE)

            grid::grid.newpage()
            grid::pushViewport(grid::viewport(layout = grid::grid.layout(h, w)))

            for (i in seq_along(cells)) {
                row <- ceiling(i / w)
                col <- i - (row - 1) * w
                is_free <- !is.na(center_idx) && i == center_idx

                # clip = "on" ensures text can never bleed into a neighboring cell
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

                fit <- fit_cell_text(cells[i], base_fontsize = if (is_free) 18 else 14)

                grid::grid.text(
                    fit$text,
                    gp = grid::gpar(
                        fontsize = fit$fontsize,
                        fontface = if (is_free) "bold" else "plain"
                    )
                )

                grid::upViewport()
            }
        },
        contentType = "image/png"
    )
}

shinyApp(ui = ui, server = server)

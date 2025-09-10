#' Time Series Principal Component Analysis Block
#'
#' Perform PCA on multivariate time series for dimension reduction
#'
#' @param n_components Integer. Number of principal components to extract (default: 2).
#' @param standardize Logical. Whether to standardize before PCA (default: TRUE).
#' @param ... Additional arguments passed to new_ts_transform_block()
#'
#' @return A ts_pca_block object
#' @export
new_ts_pca_block <- function(n_components = 2, standardize = TRUE, ...) {
  
  # Ensure n_components is an integer
  n_components <- as.integer(n_components)
  
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          
          # Reactive values
          r_n_components <- reactiveVal(n_components)
          r_standardize <- reactiveVal(standardize)
          
          # Observers
          observeEvent(input$n_components, {
            r_n_components(as.integer(input$n_components))
          })
          
          observeEvent(input$standardize, {
            r_standardize(input$standardize)
          })
          
          # Dynamic info text
          output$pca_info <- renderUI({
            n_comp <- r_n_components()
            std <- r_standardize()
            
            div(
              helpText(
                icon("project-diagram"),
                sprintf("Extracting %d principal component%s",
                        n_comp, ifelse(n_comp == 1, "", "s"))
              ),
              helpText(
                class = "text-muted",
                if (std) {
                  "Data will be standardized (mean=0, sd=1) before PCA"
                } else {
                  "Using original scale (no standardization)"
                }
              )
            )
          })
          
          list(
            expr = reactive({
              n_comp <- r_n_components()
              std <- r_standardize()
              
              # Use ts_prcomp for PCA
              # Note: ts_prcomp doesn't properly support scale parameter, 
              # so we standardize manually if needed
              if (std) {
                expr_text <- glue::glue("
                {{
                  # Standardize data manually before PCA
                  data_std <- tsbox::ts_scale(data)
                  
                  # Perform PCA
                  pca_result <- tsbox::ts_prcomp(data_std)
                  
                  # Select requested number of components
                  tbl_result <- tsbox::ts_tbl(pca_result)
                  components <- unique(tbl_result$id)[1:min({n_comp}, length(unique(tbl_result$id)))]
                  
                  # Filter to selected components
                  tbl_result[tbl_result$id %in% components, ]
                }}")
              } else {
                expr_text <- glue::glue("
                {{
                  # Perform PCA without standardization
                  pca_result <- tsbox::ts_prcomp(data)
                  
                  # Select requested number of components
                  tbl_result <- tsbox::ts_tbl(pca_result)
                  components <- unique(tbl_result$id)[1:min({n_comp}, length(unique(tbl_result$id)))]
                  
                  # Filter to selected components
                  tbl_result[tbl_result$id %in% components, ]
                }}")
              }
              
              parse(text = expr_text)[[1]]
            }),
            state = list(
              n_components = r_n_components,
              standardize = r_standardize
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        div(
          class = "ts-block-container",
          div(
            class = "ts-block-form-grid",
            
            div(
              class = "ts-block-section",
              tags$h4("PCA Settings"),
              
              div(
                class = "ts-block-input-wrapper",
                numericInput(
                  NS(id, "n_components"),
                  label = "Number of Components",
                  value = n_components,
                  min = 1,
                  max = 10,
                  step = 1
                )
              ),
              
              div(
                class = "ts-block-input-wrapper",
                checkboxInput(
                  NS(id, "standardize"),
                  label = "Standardize before PCA",
                  value = standardize
                )
              ),
              
              div(
                class = "ts-block-info",
                uiOutput(NS(id, "pca_info"))
              ),
              
              div(
                class = "alert alert-info",
                icon("info-circle"),
                " PCA requires multivariate time series data (e.g., EuStockMarkets)."
              )
            )
          )
        )
      )
    },
    class = c("ts_pca_block"),
    ...
  )
}
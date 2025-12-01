### inline output to save vertical space
inlineInput <- function(label, input, label_width = "100px", gap = "5px") {
  div(
    style = "display: flex; align-items: baseline;", # margin-bottom: 5px;
    tags$label(label, style = paste0("margin-right: ", gap, "; width:", label_width)), # margin-bottom: 0
    input
  )
}
# inlineInput("Plot width:",
#   numericInput("width", NULL, 6, min = 0.1, step = 0.1, width = 70)
# )


### group inputs. similar to wellPanel() but with no border
inputGroup <- function(..., margin_bottom = 0) {
  div(
    style = paste0("margin-bottom: ",margin_bottom,"px;"),  # adjust spacing once
    ...
  )
}


test_ggplot <- function(){
    ggplot(data.frame(x=c(1,2), y=c(2,1))) + geom_point(aes(x=x,y=y))
}

VlnPlot.xlabel <- function(
  object,
  gene,
  group.by,
  title = "",
  assay = NULL,
  slot = "data",
  log_scale = FALSE,
  colors = NULL,
  split.by = NULL,
  spread = NULL,
  jitter_pts = TRUE,
  plot_mean = TRUE,
  size = 0.5,
  sig = 3,
  number_labels = TRUE,
  text_sizes = c(13.2, 11, 8.8, 11, 8.8, 8, 3), # c(15, 10, 7, 10, 7, 7, 2),
  alpha = 0.5,
  theme = "classic"
) {
  split.by <- split.by %||% character(0)
  meta_cols <- c(group.by, split.by)
  meta_cols <- meta_cols[meta_cols %in% colnames(object@meta.data)]
  df <- object@meta.data[, meta_cols, drop = FALSE]

  assay <- assay %||% DefaultAssay(object = object)
  DefaultAssay(object = object) <- assay

  gene_exp <- FetchData(object = object, vars = gene, slot = slot)
  if (sum(gene_exp) == 0) {
    warning("No expression in data")
    return(invisible(NULL))
  }

  colnames(gene_exp) <- "value"
  df <- cbind(df, gene_exp)

  colnames(df) <- gsub("-", "", colnames(df))
  gene <- gsub("-", "", gene)
  group.by <- gsub("-", "", group.by)
  if (length(split.by) > 0) {
    split.by <- gsub("-", "", split.by)
  }
  if (!is.null(spread)) {
    spread <- gsub("-", "", spread)
  }

  if (!is.null(spread) && length(spread) >= 2 && spread[1] %in% colnames(df)) {
    target_col <- spread[1]
    target_val <- spread[2]
    column_values <- df[[target_col]]
    ind <- which(!is.na(column_values) & column_values == target_val)

    if (length(ind) > 0) {
      others <- setdiff(unique(column_values), target_val)
      rmdf <- df[ind, , drop = FALSE]
      df <- df[-ind, , drop = FALSE]

      for (other in others) {
        rmdf[[target_col]] <- other
        df <- rbind(df, rmdf)
      }
    }
  }

  df$plot <- if (isTRUE(log_scale)) log2(df$value + 1) else df$value

  g <- ggplot(df)

  if (!is.null(colors)) {
    g <- g + scale_color_manual(values = colors)
    g <- g + scale_fill_manual(values = colors)
  }

  g <- g + switch(
    theme,
    bw = theme_bw(),
    theme_classic()
  )

  if (identical(title, "")) {
    title <- gene
  }

  g <- g + labs(title = title, y = gene)
  g <- g + theme(
    plot.title = element_text(size = text_sizes[1], hjust = 0.5),
    axis.title = element_text(size = text_sizes[2]),
    axis.text = element_text(size = text_sizes[3]),
    legend.title = element_text(size = text_sizes[4]),
    legend.text = element_text(size = text_sizes[5]),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(hjust = 1, vjust = 1, angle = 90)
  )

  if (isTRUE(jitter_pts)) {
    g <- g + geom_jitter(
      aes_string(x = group.by, y = "plot", col = group.by),
      width = 0.2,
      size = size
    )
  }

  g <- g + geom_violin(
    aes_string(x = group.by, y = "plot", fill = group.by),
    colour = "black",
    trim = TRUE,
    scale = "width",
    alpha = alpha,
    linewidth = 0.3
  )

  if (isTRUE(number_labels)) {
    g <- g + stat_summary(
      aes_string(x = group.by, y = "value"),
      fun.data = function(x) data.frame(y = -max(df$plot) / 25, label = length(x)),
      colour = "black",
      geom = "text",
      size = text_sizes[7]
    )

    g <- g + stat_summary(
      aes_string(x = group.by, y = "value"),
      fun.data = function(x) data.frame(y = -max(df$plot) / 10, label = round(mean(as.numeric(x > 0)), sig)),
      colour = "black",
      geom = "text",
      size = text_sizes[7]
    )
  }

  if (isTRUE(plot_mean)) {
    group_vars <- df[, colnames(df) %in% c(group.by, split.by), drop = FALSE]
    mean_by_group <- tapply(df$value, INDEX = as.list(group_vars), FUN = mean)
    plot_max <- max(df$plot, na.rm = TRUE)
    mean_max <- suppressWarnings(max(mean_by_group, na.rm = TRUE))
    mean_max <- if (!is.finite(mean_max) || mean_max == 0) 1 else mean_max
    scale <- plot_max / mean_max

    if (is.finite(scale) && scale > 0) {
      g <- g + suppressWarnings(stat_summary(
        aes_string(x = group.by, y = "value"),
        fun = function(x) mean(x) * (scale * 0.5),
        colour = "black",
        geom = "point",
        size = 2,
        na.rm = TRUE
      ))

      g <- g + scale_y_continuous(
        sec.axis = sec_axis(~ ./(scale * 0.5), name = "Mean Expression")
      )
    }
  }

  if (length(split.by) == 1) {
    g <- g + facet_grid(
      cols = vars(!!sym(split.by)),
      scales = "free_x",
      space = "free_x"
    )
  } else if (length(split.by) == 2) {
    g <- g + facet_grid(
      rows = vars(!!sym(split.by[2])),
      cols = vars(!!sym(split.by[1])),
      scales = "free_x",
      space = "free_x"
    )
  } else if (length(split.by) > 2) {
    stop("Parameter split.by must contain at most two variables.")
  }

  if (length(split.by) > 0) {
    g <- g + theme(strip.text = element_text(size = text_sizes[6]))
  }

  g
}




HeatmapPseodoBulk <- function(tb, md, genes=NULL, groupby, splitby = NULL, cluster_genes=FALSE){
  if (!is.null(genes)) {
    tb <- tb[genes, ]
  }
  # aggregate expression by groupby and splitby
  df <- md[,c(groupby, splitby), drop=FALSE]
  df$group <- apply(df, 1, function(x) paste0(x, collapse = ":"))
  groups <- split(1:nrow(df), df$group)
  agg_tb <- sapply(groups, function(idx){
    rowSums(tb[,idx,drop=FALSE])
  })
  agg_md <- as.data.frame(do.call(rbind, strsplit(colnames(agg_tb), ":")))
  colnames(agg_md) <- c(groupby, splitby)
  agg_md[,groupby] <- factor(agg_md[,groupby], levels = levels(md[,groupby]))
  if (!is.null(splitby)){
    agg_md[,splitby] <- factor(agg_md[,splitby], levels = levels(md[,splitby]))
  }

  if (!is.null(splitby)){
    col_splitby <- agg_md[,splitby]
  }else{
    col_splitby <- NULL
  }

  agg_tb_norm <- apply(agg_tb, 2, function(x) x / sum(x) * 1e6)
  agg_tb_norm_scaled <- t(scale(t(agg_tb_norm)))

  hmap <- ComplexHeatmap::Heatmap(agg_tb_norm_scaled,
    name = "Expression",
    cluster_rows = cluster_genes,
    cluster_columns = FALSE,
    show_row_names = TRUE,
    show_column_names = TRUE,
    show_row_dend = FALSE,
    row_names_side = "left",
    column_split = col_splitby,
    column_order = order(agg_md[, groupby])
  )
  return(hmap)
}
# tb <- readRDS('/Users/yuqing/UMass Medical School Dropbox/Yuqing Wang/Ongoing/data_hosting/shinyApp_content/fourDisease_indrop/fourDisease_indrop_pseudobulk_sum_CellType_Disease_Skin.rds')        
# md <- as.data.frame(do.call(rbind, strsplit(colnames(tb), ":")))
# colnames(md) <- c("CellType","Disease","Skin")
# md$colName <- colnames(tb)
# md$Disease <- factor(md$Disease, levels=c("HC","DM","CLE","Pso","Vit"))
# md$CellType <- factor(md$CellType, levels = c("MC","Lymph","KC","Mel"))
# md$Skin <- factor(md$Skin, levels = c("H","NL","L"))

# groupby <- "Skin"
# splitby <- "Disease"
# splitby <- NULL

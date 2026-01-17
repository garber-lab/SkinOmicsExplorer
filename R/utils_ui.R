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
    style = paste0("margin-bottom: ", margin_bottom, "px;"), # adjust spacing once
    ...
  )
}

manual_scale_values <- function(values, data_values) {
  if (is.null(values)) {
    return(values)
  }
  value_names <- names(values)
  if (is.null(value_names) || all(value_names == "")) {
    return(values)
  }

  data_levels <- if (is.factor(data_values)) {
    levels(droplevels(data_values))
  } else {
    unique(as.character(data_values))
  }
  data_levels <- data_levels[!is.na(data_levels)]
  if (length(data_levels) == 0) {
    return(unname(values))
  }

  if (!any(data_levels %in% value_names)) {
    if (length(values) == length(data_levels)) {
      values <- stats::setNames(unname(values), data_levels)
    } else {
      values <- unname(values)
    }
  }

  values
}

FetchData.compat <- function(object, vars, slot = NULL, ...) {
  fetch_formals <- tryCatch(formals(FetchData), error = function(e) NULL)
  if (!is.null(fetch_formals) && "layer" %in% names(fetch_formals)) {
    FetchData(object = object, vars = vars, layer = slot, ...)
  } else {
    FetchData(object = object, vars = vars, slot = slot, ...)
  }
}


test_ggplot <- function() {
  ggplot(data.frame(x = c(1, 2), y = c(2, 1))) +
    geom_point(aes(x = x, y = y))
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
    theme = "classic") {
  split.by <- split.by %||% character(0)
  meta_cols <- c(group.by, split.by)
  meta_cols <- meta_cols[meta_cols %in% colnames(object@meta.data)]
  df <- object@meta.data[, meta_cols, drop = FALSE]

  assay <- assay %||% DefaultAssay(object = object)
  DefaultAssay(object = object) <- assay

  gene_exp <- FetchData.compat(object = object, vars = gene, slot = slot)
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
    colors <- manual_scale_values(colors, df[[group.by]])
    g <- g + scale_color_manual(values = colors)
    g <- g + scale_fill_manual(values = colors)
  }

  g <- g + switch(theme,
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
      aes(x = .data[[group.by]], y = .data[["plot"]], col = .data[[group.by]]),
      width = 0.2,
      size = size
    )
  }

  g <- g + geom_violin(
    aes(x = .data[[group.by]], y = .data[["plot"]], fill = .data[[group.by]]),
    colour = "black",
    trim = TRUE,
    scale = "width",
    alpha = alpha,
    linewidth = 0.3
  )

  if (isTRUE(number_labels)) {
    g <- g + stat_summary(
      aes(x = .data[[group.by]], y = .data[["value"]]),
      fun.data = function(x) data.frame(y = -max(df$plot) / 25, label = length(x)),
      colour = "black",
      geom = "text",
      size = text_sizes[7]
    )

    g <- g + stat_summary(
      aes(x = .data[[group.by]], y = .data[["value"]]),
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
        aes(x = .data[[group.by]], y = .data[["value"]]),
        fun = function(x) mean(x) * (scale * 0.5),
        colour = "black",
        geom = "point",
        size = 2,
        na.rm = TRUE
      ))

      g <- g + scale_y_continuous(
        sec.axis = sec_axis(~ . / (scale * 0.5), name = "Mean Expression")
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




HeatmapPseudoBulk <- function(tb, md, genes = NULL, groupby, groupby.order = NULL, splitby = NULL, splitby.order = NULL, cluster_genes = FALSE) {
  libsize <- colSums(tb)
  if (!is.null(genes)) {
    tb <- tb[genes, , drop = FALSE]
  }
  # aggregate expression by groupby and splitby (no bins)
  group_cols <- c(groupby, splitby)
  df <- md[, group_cols, drop = FALSE]
  key <- apply(df, 1, paste0, collapse = ":")

  agg_tb <- t(rowsum(t(tb), group = key, reorder = FALSE))
  agg_libsize <- rowsum(libsize, group = key, reorder = FALSE)[, 1]

  agg_md <- df[match(colnames(agg_tb), key), , drop = FALSE]
  rownames(agg_md) <- colnames(agg_tb)

  if (!is.null(groupby.order)) {
    agg_md[[groupby]] <- factor(agg_md[[groupby]], levels = groupby.order)
  }
  if (!is.null(splitby) && !is.null(splitby.order)) {
    agg_md[[splitby]] <- factor(agg_md[[splitby]], levels = splitby.order)
  }

  agg_tb_norm <- sweep(agg_tb, 2, agg_libsize, "/") * 1e6

  ordered_cols <- colnames(agg_tb_norm)
  if (length(ordered_cols) > 0) {
    if (is.factor(agg_md[[groupby]])) {
      group_rank <- as.integer(agg_md[[groupby]])
    } else {
      group_rank <- as.integer(factor(agg_md[[groupby]]))
    }

    has_splitby <- !is.null(splitby) && splitby %in% colnames(agg_md)
    if (has_splitby) {
      if (is.factor(agg_md[[splitby]])) {
        split_rank <- as.integer(agg_md[[splitby]])
      } else {
        split_rank <- as.integer(factor(agg_md[[splitby]]))
      }
      ordered_cols <- ordered_cols[order(group_rank, split_rank)]
    } else {
      ordered_cols <- ordered_cols[order(group_rank)]
    }
  }

  mat_scaled <- t(scale(t(agg_tb_norm[, ordered_cols, drop = FALSE])))
  colnames(mat_scaled) <- as.character(agg_md[ordered_cols, groupby])

  col_splitby <- NULL
  if (!is.null(splitby) && splitby %in% colnames(agg_md)) {
    col_splitby <- agg_md[ordered_cols, splitby]
    if (is.factor(col_splitby)) {
      col_splitby <- droplevels(col_splitby)
    }
  }

  col_fun <- circlize::colorRamp2(
    breaks = c(-2, 0, 2),
    colors = c("blue", "white", "red")
  )

  hmap <- ComplexHeatmap::Heatmap(
    mat_scaled,
    name = "Expression",
    col = col_fun,
    cluster_rows = cluster_genes,
    cluster_columns = FALSE,
    show_row_names = TRUE,
    show_column_names = TRUE,
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    row_names_side = "left",
    column_split = col_splitby
  )
  return(hmap)
}

HeatmapBulk <- function(tb_cpm, md, genes = NULL, groupby, groupby.order = NULL, splitby = NULL, splitby.order = NULL, cluster_genes = FALSE) {
  if (!is.null(genes)) {
    tb_cpm <- tb_cpm[genes, , drop = FALSE]
  }

  if (!is.null(groupby.order) && groupby %in% colnames(md)) {
    md[[groupby]] <- factor(md[[groupby]], levels = groupby.order)
  }
  if (!is.null(splitby) && !is.null(splitby.order) && splitby %in% colnames(md)) {
    md[[splitby]] <- factor(md[[splitby]], levels = splitby.order)
  }

  ordered_cols <- colnames(tb_cpm)
  if (length(ordered_cols) > 0 && groupby %in% colnames(md)) {
    if (is.factor(md[[groupby]])) {
      group_rank <- as.integer(md[[groupby]])
    } else {
      group_rank <- as.integer(factor(md[[groupby]]))
    }

    has_splitby <- !is.null(splitby) && splitby %in% colnames(md)
    if (has_splitby) {
      if (is.factor(md[[splitby]])) {
        split_rank <- as.integer(md[[splitby]])
      } else {
        split_rank <- as.integer(factor(md[[splitby]]))
      }
      ordered_cols <- ordered_cols[order(group_rank, split_rank)]
    } else {
      ordered_cols <- ordered_cols[order(group_rank)]
    }
  }

  mat_scaled <- t(scale(t(tb_cpm[, ordered_cols, drop = FALSE])))
  if (groupby %in% colnames(md)) {
    colnames(mat_scaled) <- as.character(md[ordered_cols, groupby])
  }

  col_splitby <- NULL
  if (!is.null(splitby) && splitby %in% colnames(md)) {
    col_splitby <- md[ordered_cols, splitby]
    if (is.factor(col_splitby)) {
      col_splitby <- droplevels(col_splitby)
    }
  }

  magma_colors <- viridis::magma(256)
  col_fun <- circlize::colorRamp2(
    breaks = seq(-2, 2, length.out = length(magma_colors)),
    colors = magma_colors
  )

  hmap <- ComplexHeatmap::Heatmap(
    mat_scaled,
    name = "CPM",
    col = col_fun,
    cluster_rows = cluster_genes,
    cluster_columns = FALSE,
    show_row_names = TRUE,
    show_column_names = FALSE,
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    row_names_side = "left",
    column_split = col_splitby,
    column_title_side = "bottom",
    column_title_gp = grid::gpar(fontsize = 8)
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





# this is similar to HeatmapPseudoBulk but add binned cell types on the left side
HeatmapPseudoBulk_bin <- function(tb, md, genes = NULL, groupby, groupby.order = NULL, splitby = NULL, splitby.order = NULL, cluster_genes = FALSE, show_bins = TRUE) {
  # colnames of tb and rownames of md should match
  libsize <- colSums(tb)
  if (!is.null(genes)) {
    tb <- tb[genes, , drop = FALSE]
  }

  bin_col <- NULL
  if (isTRUE(show_bins) && "bin" %in% colnames(md)) {
    bin_pattern <- "_bin[0-9]+$"
    md_cols_no_bin <- setdiff(colnames(md), "bin")
    groupby_idx <- match(groupby, md_cols_no_bin)
    checked_groupby_parts <- FALSE
    bin_active <- FALSE
    if (!is.na(groupby_idx)) {
      col_parts <- strsplit(colnames(tb), ":")
      part_lengths <- vapply(col_parts, length, integer(1))
      if (length(unique(part_lengths)) == 1) {
        parts_mat <- do.call(rbind, col_parts)
        if (ncol(parts_mat) == length(md_cols_no_bin)) {
          checked_groupby_parts <- TRUE
          bin_active <- any(grepl(bin_pattern, parts_mat[, groupby_idx]))
        }
      }
    }
    if (!checked_groupby_parts) {
      bin_vals <- md[["bin"]]
      if (is.factor(bin_vals)) {
        bin_vals <- as.character(bin_vals)
      }
      bin_active <- any(!is.na(bin_vals) & bin_vals != "")
    }
    if (bin_active) {
      bin_col <- "bin"
    }
  }

  group_cols <- c(groupby, bin_col, splitby)
  df <- md[, group_cols, drop = FALSE]
  key <- apply(df, 1, paste0, collapse = ":")

  agg_tb <- t(rowsum(t(tb), group = key, reorder = FALSE))
  agg_libsize <- rowsum(libsize, group = key, reorder = FALSE)[, 1]

  agg_md <- df[match(colnames(agg_tb), key), , drop = FALSE]
  rownames(agg_md) <- colnames(agg_tb)

  has_bin <- !is.null(bin_col) && bin_col %in% colnames(agg_md)
  bin_vals <- if (has_bin) agg_md[[bin_col]] else NULL
  if (is.factor(bin_vals)) {
    bin_vals <- as.character(bin_vals)
  }
  is_bin <- if (has_bin) !is.na(bin_vals) & bin_vals != "" else rep(FALSE, nrow(agg_md))

  agg_tb_norm <- matrix(0, nrow = nrow(agg_tb), ncol = ncol(agg_tb), dimnames = dimnames(agg_tb))
  if (any(!is_bin)) {
    agg_tb_norm[, !is_bin] <- sweep(agg_tb[, !is_bin, drop = FALSE], 2, agg_libsize[!is_bin], "/") * 1e6
  }
  if (any(is_bin)) {
    # Normalize bins together per condition, then scale by per-condition bin count.
    cond_df <- agg_md[, setdiff(group_cols, bin_col), drop = FALSE]
    cond_key <- apply(cond_df, 1, paste0, collapse = ":")
    cond_key_bin <- cond_key[is_bin]

    total_libsize_bin <- tapply(agg_libsize[is_bin], cond_key_bin, sum)
    bin_counts <- tapply(agg_md[[bin_col]][is_bin], cond_key_bin, function(x) {
      x <- if (is.factor(x)) as.character(x) else x
      sum(!is.na(x) & x != "")
    })

    agg_tb_norm[, is_bin] <- sweep(agg_tb[, is_bin, drop = FALSE], 2, total_libsize_bin[cond_key_bin], "/") * 1e6
    agg_tb_norm[, is_bin] <- sweep(agg_tb_norm[, is_bin, drop = FALSE], 2, bin_counts[cond_key_bin], "*")
  }

  has_splitby <- !is.null(splitby) && splitby %in% colnames(agg_md)
  bin_groups <- list()
  if (any(is_bin)) {
    bin_info <- data.frame(
      col = colnames(agg_tb_norm)[is_bin],
      base = as.character(agg_md[is_bin, groupby]),
      bin = as.character(agg_md[is_bin, bin_col]),
      stringsAsFactors = FALSE
    )
    if (has_splitby) {
      bin_info$split <- as.character(agg_md[is_bin, splitby])
    }

    base_levels <- if (is.factor(agg_md[[groupby]])) {
      levels(agg_md[[groupby]])
    } else {
      unique(bin_info$base)
    }
    base_levels <- base_levels[base_levels %in% bin_info$base]

    bin_levels <- if (is.factor(agg_md[[bin_col]])) levels(agg_md[[bin_col]]) else NULL
    split_levels <- if (has_splitby && is.factor(agg_md[[splitby]])) levels(agg_md[[splitby]]) else NULL

    bin_groups <- lapply(base_levels, function(ct) {
      info <- bin_info[bin_info$base == ct, , drop = FALSE]
      bin_rank <- if (!is.null(bin_levels)) {
        match(info$bin, bin_levels)
      } else {
        bin_num <- suppressWarnings(as.integer(sub("^[^0-9]*", "", info$bin)))
        if (all(!is.na(bin_num))) {
          bin_num
        } else {
          as.integer(factor(info$bin, levels = unique(info$bin)))
        }
      }
      if (has_splitby && "split" %in% colnames(info)) {
        split_rank <- if (!is.null(split_levels)) {
          match(info$split, split_levels)
        } else {
          as.integer(factor(info$split, levels = unique(info$split)))
        }
        info <- info[order(bin_rank, split_rank), , drop = FALSE]
      } else {
        info <- info[order(bin_rank), , drop = FALSE]
      }
      info$col
    })
    names(bin_groups) <- base_levels
  }

  other_cols <- colnames(agg_tb_norm)[!is_bin]
  if (length(other_cols) > 0) {
    other_md <- agg_md[other_cols, , drop = FALSE]
    if (is.factor(other_md[[groupby]])) {
      group_rank <- as.integer(other_md[[groupby]])
    } else {
      group_rank <- as.integer(factor(other_md[[groupby]]))
    }

    if (has_splitby) {
      if (is.factor(other_md[[splitby]])) {
        split_rank <- as.integer(other_md[[splitby]])
      } else {
        split_rank <- as.integer(factor(other_md[[splitby]]))
      }
      other_cols <- other_cols[order(group_rank, split_rank)]
    } else {
      other_cols <- other_cols[order(group_rank)]
    }
  }

  ordered_cols <- c(unlist(bin_groups, use.names = FALSE), other_cols)
  mat_scaled <- t(scale(t(agg_tb_norm[, ordered_cols, drop = FALSE])))

  col_fun <- circlize::colorRamp2(
    breaks = c(-2, 0, 2),
    colors = c("blue", "white", "red")
  )

  heatmaps <- list()
  roworder <- NULL
  show_rows <- TRUE
  if (length(bin_groups) > 0) {
    for (i in seq_along(bin_groups)) {
      cols <- bin_groups[[i]]
      bin_mat <- mat_scaled[, cols, drop = FALSE]
      if (!is.null(roworder)) {
        bin_mat <- bin_mat[roworder, , drop = FALSE]
      }
      hmap <- ComplexHeatmap::Heatmap(
        bin_mat,
        name = "Expression",
        col = col_fun,
        cluster_rows = cluster_genes,
        cluster_columns = FALSE,
        show_row_names = show_rows,
        show_column_names = FALSE,
        show_row_dend = FALSE,
        row_names_side = "left",
        column_title = names(bin_groups)[i],
        width = grid::unit(2, "cm")
      )
      heatmaps[[length(heatmaps) + 1]] <- hmap
      roworder <- hmap@row_order
      show_rows <- FALSE
      cluster_genes <- FALSE
    }
  }

  if (length(other_cols) > 0) {
    other_mat <- mat_scaled[, other_cols, drop = FALSE]
    colnames(other_mat) <- as.character(agg_md[other_cols, groupby])
    col_splitby <- if (has_splitby) agg_md[other_cols, splitby] else NULL
    if (is.factor(col_splitby)) {
      col_splitby <- droplevels(col_splitby)
    }
    hmap_others <- ComplexHeatmap::Heatmap(
      other_mat,
      name = "Expression",
      col = col_fun,
      cluster_rows = cluster_genes,
      cluster_columns = FALSE,
      show_row_names = show_rows,
      show_column_names = TRUE,
      show_row_dend = FALSE,
      show_column_dend = FALSE,
      row_names_side = "left",
      column_split = col_splitby
    )
    heatmaps[[length(heatmaps) + 1]] <- hmap_others
  }

  if (length(heatmaps) == 1) {
    return(heatmaps[[1]])
  }
  Reduce(`+`, heatmaps)
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








ImageDimPlot.ssc <- function(object, fov, group.by = NULL, split.by = NULL, size = 0.1,
                             cols = NULL, alpha = 1, highlight.by = NULL, highlight.groups = NULL,
                             highlight.size = 0.2, highlight.cols = NULL, highlight.alpha = 1,
                             molecules = NULL, molecules.size = 0.1, molecules.cols = NULL,
                             molecules.alpha = 1, legend.concise = TRUE, dark.background = T, crop = NULL, flip = F,
                             scalebar.length = NULL, scalebar.numConv = 1, scalebar.unit = NULL, scalebar.position = "bottomright",
                             scalebar.color = NULL, scalebar.text.size = 3, scalebar.margin = 0.03) {
  if (is.null(group.by)) {
    group.by <- "ident"
  }

  plotted_molecules <- character(0)

  fov_image <- object@images[[fov]]
  coords <- fov_image$centroids@coords
  df <- data.frame(
    x = coords[, 1],
    y = coords[, 2],
    stringsAsFactors = FALSE
  )

  ind.fov <- match(fov_image$centroids@cells, colnames(object))
  df.meta <- FetchData(object = object, vars = c(group.by, split.by), cells = ind.fov)

  if (is.null(highlight.by)) {
    highlight.by <- group.by
  } else if (!identical(highlight.by, group.by)) {
    df.meta[[highlight.by]] <- object@meta.data[ind.fov, highlight.by]
  }

  df.meta$highlight <- ifelse(df.meta[[highlight.by]] %in% highlight.groups, "y", "n")
  df <- cbind(df, df.meta)

  if (!is.null(molecules)) {
    valid_molecules <- molecules[molecules %in% rownames(object)]
    missing <- setdiff(molecules, valid_molecules)
    if (length(missing) > 0) {
      message("Molecules not in the object: ", paste(missing, collapse = ","))
    }
    if (length(valid_molecules) > 0) {
      molecule_coords <- fov_image@molecules$molecules[valid_molecules]
      df.mol <- do.call(rbind, lapply(names(molecule_coords), function(name) {
        mol_coords <- molecule_coords[[name]]@coords
        data.frame(
          x = mol_coords[, 1],
          y = mol_coords[, 2],
          mol = name,
          stringsAsFactors = FALSE
        )
      }))
      colnames(df.mol)[3] <- group.by

      if (!is.null(split.by)) {
        split_values <- unique(df[[split.by]])
        df_mol_base <- df.mol
        df.mol <- do.call(rbind, lapply(split_values, function(val) {
          tmp <- df_mol_base
          tmp[[split.by]] <- val
          tmp
        }))
      }

      df.mol$highlight <- "m"
      df <- rbind(df, df.mol)
    }
  }

  plot_xlim <- range(df$x, na.rm = TRUE)
  plot_ylim <- range(df$y, na.rm = TRUE)

  g <- ggplot(df) +
    geom_point(aes(x = .data[["x"]], y = .data[["y"]],
                   alpha = .data[["highlight"]], size = .data[["highlight"]],
                   colour = .data[[group.by]]),
      shape = 16
    ) +
    theme_classic() +
    theme(
      axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
      axis.line = element_blank(), panel.grid = element_blank()
    ) +
    guides(colour = guide_legend(title = group.by), size = "none", alpha = "none") +
    scale_size_manual(values = c(y = highlight.size, n = size, m = molecules.size)) +
    scale_alpha_manual(values = c(y = highlight.alpha, n = alpha, m = molecules.alpha))

  g2 <- ggplot_build(g)
  color_scale <- g2$plot$scales$get_scales("colour")
  cols.default <- color_scale$palette.cache
  names(cols.default) <- sort(unique(df[[group.by]]))

  legend_breaks <- NULL
  if (isTRUE(legend.concise)) {
    legend_breaks <- c()
    if (!is.null(highlight.groups)) {
      legend_breaks <- c(legend_breaks, highlight.groups)
    }
    if (!is.null(molecules) && length(valid_molecules) > 0) {
      legend_breaks <- c(legend_breaks, valid_molecules)
    }
    legend_breaks <- as.character(unique(legend_breaks))
    legend_breaks <- legend_breaks[legend_breaks %in% names(cols.default)]
  }

  effective.cols <- cols
  if (!is.null(highlight.cols)) {
    if (is.null(effective.cols)) {
      effective.cols <- cols.default
    }
    effective.cols[names(highlight.cols)] <- highlight.cols
  }
  if (!is.null(molecules.cols)) {
    if (is.null(effective.cols)) {
      effective.cols <- cols.default
    }
    effective.cols[names(molecules.cols)] <- molecules.cols
  }
  if (!is.null(effective.cols)) {
    effective.cols <- manual_scale_values(effective.cols, df[[group.by]])
    if (is.null(legend_breaks)) {
      g <- g + scale_color_manual(values = effective.cols)
    } else {
      g <- g + scale_color_manual(values = effective.cols, breaks = legend_breaks)
    }
  } else if (!is.null(legend_breaks)) {
    g <- g + scale_color_discrete(breaks = legend_breaks)
  }

  if (!is.null(split.by)) {
    g <- g + facet_wrap(reformulate(split.by))
  }

  coord_args <- list(ratio = 1)
  if (!is.null(crop)) {
    if (identical(crop, TRUE)) {
      crop <- c(min(df$x) - 1, max(df$x) + 1, min(df$y) - 1, max(df$y) + 1)
    } else if (length(crop) != 4) {
      stop("crop must be TRUE or a numeric vector c(min.x, max.x, min.y, max.y).")
    }
    plot_xlim <- crop[1:2]
    plot_ylim <- crop[3:4]
  }

  if (dark.background) {
    g <- g + theme(panel.background = element_rect(fill = "black", colour = "black"))
  }

  if (!is.null(scalebar.length)) {
    if (!is.numeric(scalebar.length) || length(scalebar.length) != 1 || scalebar.length <= 0) {
      stop("scalebar.length must be a positive numeric value.")
    }

    span_x <- diff(plot_xlim)
    span_y <- diff(plot_ylim)
    margin <- max(scalebar.margin, 0)
    margin_x <- span_x * margin
    margin_y <- span_y * margin
    offset_y <- if (span_y > 0) span_y * 0.02 else span_x * 0.02
    bar_colour <- if (is.null(scalebar.color)) {
      if (isTRUE(dark.background)) "white" else "black"
    } else {
      scalebar.color
    }

    if (is.character(scalebar.position)) {
      pos <- match.arg(scalebar.position, c("bottomright", "bottomleft", "topright", "topleft"))
      if (scalebar.length > span_x) {
        warning("scalebar.length exceeds the x-range of the plot and may be clipped.", call. = FALSE)
      }

      from_left <- grepl("left", pos)
      from_bottom <- grepl("bottom", pos)

      x_start <- if (from_left) plot_xlim[1] + margin_x else plot_xlim[2] - margin_x - scalebar.length
      y_start <- if (from_bottom) plot_ylim[1] + margin_y else plot_ylim[2] - margin_y
      label_y <- if (from_bottom) y_start + offset_y else y_start - offset_y
      text_vjust <- if (from_bottom) 0 else 1
    } else if (is.numeric(scalebar.position) && length(scalebar.position) == 2) {
      x_start <- scalebar.position[1]
      y_start <- scalebar.position[2]
      label_y <- y_start + offset_y
      text_vjust <- 0
    } else {
      stop("scalebar.position must be 'bottomright', 'bottomleft', 'topright', 'topleft', or a numeric length-2 vector.")
    }

    x_end <- x_start + scalebar.length
    scalebar.label <- round(scalebar.length * scalebar.numConv, digits = 2)
    label <- if (is.null(scalebar.unit) || scalebar.unit == "") {
      scalebar.label
    } else {
      paste(scalebar.label, scalebar.unit)
    }

    g <- g +
      annotate("segment", x = x_start, xend = x_end, y = y_start, yend = y_start, colour = bar_colour, linewidth = 0.5) +
      annotate("text", x = (x_start + x_end) / 2, y = label_y, label = label, colour = bar_colour, size = scalebar.text.size, vjust = text_vjust)
  }

  if (flip) {
    g <- g + coord_flip(xlim = plot_xlim, ylim = plot_ylim)
  } else {
    g <- g + coord_fixed(ratio = 1, xlim = plot_xlim, ylim = plot_ylim)
  }

  g
}




ImageFeaturePlot.ssc <- function(object, features, fov = NULL, assay = NULL, cols = c("lightgrey", "firebrick1"),
                                 size = 0.2, alpha = 1, min.cutoff = NA, max.cutoff = NA,
                                 dark.background = T, coord.fixed = T, crop = NULL, scalebar.length = NULL, 
                                 scalebar.numConv = 1, scalebar.unit = NULL, scalebar.position = "bottomright",
                                 scalebar.color = NULL, scalebar.text.size = 3, scalebar.margin = 0.03) {
  
  if (is.null(assay)) {
    assay <- DefaultAssay(object)
  }
  
  fov_image <- object@images[[fov]]
  coords <- fov_image$centroids@coords
  df <- data.frame(
    x = coords[, 1],
    y = coords[, 2],
    stringsAsFactors = FALSE
  )
  
  ind.fov <- match(fov_image$centroids@cells, colnames(object))
  df.meta <- t(object@assays[[assay]]@layers$data[rownames(object) %in% features, ind.fov, drop = FALSE])
  colnames(df.meta) <- features

  if (!is.na(min.cutoff)) {
    df.meta <- apply(df.meta, 2, function(x) {
      x[x < min.cutoff] <- min.cutoff
    })
  }
  if (!is.na(max.cutoff)) {
    df.meta <- apply(df.meta, 2, function(x) {
      x[x > max.cutoff] <- max.cutoff
    })
  }
  
  if (!is.null(scalebar.length)) {
    if (!is.numeric(scalebar.length) || length(scalebar.length) != 1 || scalebar.length <= 0) {
      stop("scalebar.length must be a positive numeric value.")
    }
  }
  
  g_list <- list()
  for (i in seq_along(features)){
    feature <- features[i]
    df$feature_value <- df.meta[, feature]

    plot_xlim <- range(df$x, na.rm = TRUE)
    plot_ylim <- range(df$y, na.rm = TRUE)
    
    g <- ggplot(df) +
      geom_point(aes(x = x, y = y, colour = feature_value), shape = 16, size = size, alpha = alpha) +
      theme_classic() +
      theme(
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(), panel.grid = element_blank()
      ) +
      scale_color_gradientn(colours = cols, na.value = "lightgrey", name = feature)

    coord_args <- list(ratio = 1)
    if (!is.null(crop)) {
      if (identical(crop, TRUE)) {
        crop <- c(min(df$x) - 1, max(df$x) + 1, min(df$y) - 1, max(df$y) + 1)
      } else if (length(crop) != 4) {
        stop("crop must be TRUE or a numeric vector c(min.x, max.x, min.y, max.y).")
      }
      plot_xlim <- crop[1:2]
      plot_ylim <- crop[3:4]
    }
    
    if (isTRUE(coord.fixed)) {
      g <- g + coord_fixed(ratio = 1, xlim = plot_xlim, ylim = plot_ylim)
    } else {
      g <- g + coord_cartesian(xlim = plot_xlim, ylim = plot_ylim)
    }
    
    if (dark.background) {
      g <- g + theme(panel.background = element_rect(fill = "black", colour = "black"))
    }
    
    # Add scalebar if specified
    if (!is.null(scalebar.length)) {
      span_x <- diff(plot_xlim)
      span_y <- diff(plot_ylim)
      margin <- max(scalebar.margin, 0)
      margin_x <- span_x * margin
      margin_y <- span_y * margin
      offset_y <- if (span_y > 0) span_y * 0.02 else span_x * 0.02
      bar_colour <- if (is.null(scalebar.color)) {
        if (isTRUE(dark.background)) "white" else "black"
      } else {
        scalebar.color
      }

      if (is.character(scalebar.position)) {
        pos <- match.arg(scalebar.position, c("bottomright", "bottomleft", "topright", "topleft"))
        if (scalebar.length > span_x) {
          warning("scalebar.length exceeds the x-range of the plot and may be clipped.", call. = FALSE)
        }

        from_left <- grepl("left", pos)
        from_bottom <- grepl("bottom", pos)

        x_start <- if (from_left) plot_xlim[1] + margin_x else plot_xlim[2] - margin_x - scalebar.length
        y_start <- if (from_bottom) plot_ylim[1] + margin_y else plot_ylim[2] - margin_y
        label_y <- if (from_bottom) y_start + offset_y else y_start - offset_y
        text_vjust <- if (from_bottom) 0 else 1
      } else if (is.numeric(scalebar.position) && length(scalebar.position) == 2) {
        x_start <- scalebar.position[1]
        y_start <- scalebar.position[2]
        label_y <- y_start + offset_y
        text_vjust <- 0
      } else {
        stop("scalebar.position must be 'bottomright', 'bottomleft', 'topright', 'topleft', or a numeric length-2 vector.")
      }

      x_end <- x_start + scalebar.length
      scalebar.label <- round(scalebar.length * scalebar.numConv, digits = 2)
      label <- if (is.null(scalebar.unit) || scalebar.unit == "") {
        scalebar.label
      } else {
        paste(scalebar.label, scalebar.unit)
      }

      g <- g +
        annotate("segment", x = x_start, xend = x_end, y = y_start, yend = y_start, colour = bar_colour, linewidth = 0.5) +
        annotate("text", x = (x_start + x_end) / 2, y = label_y, label = label, colour = bar_colour, size = scalebar.text.size, vjust = text_vjust)
    }
    
    g_list[[i]] <- g
  }
  
  plots <- plot_grid(plotlist = g_list, ncol = ceiling(sqrt(length(features))))
  
  return(plots)
}









library(spatstat.geom)
library(spatstat.explore)
library(dplyr)
library(ggplot2)
library(viridisLite)  # for palettes if needed
ImageFeaturePlot.contour <- function(object, feature, fov, assay = NULL,
                                     plot.all = T, group.by = NULL, size = 0.1, cols = NULL, alpha = 0.5,
                                     sigma = NULL, n_levels = 6, color_palette = NULL, threshold = 0.9, contour.alpha = 0.7,
                                     dark.background = T, scalebar.length = NULL, scalebar.numConv = 1,
                                     scalebar.unit = NULL, scalebar.position = "bottomright", scalebar.color = NULL,
                                     scalebar.text.size = 3, scalebar.margin = 0.03){
  
  if (is.null(assay)){
    assay <- DefaultAssay(object)
  }

  df <- getCoords.cell(object, fov = fov)
  df$expr <- object@assays[[assay]]@layers$data[rownames(object) == feature, match(rownames(df), colnames(object)), drop = TRUE]
  
  if (!is.null(group.by)){
    ind.fov <- match(object@images[[fov]]$centroids@cells, colnames(object))
    df$group.by <- object@meta.data[ind.fov,group.by]
  }

  if (!is.null(scalebar.length)) {
    if (!is.numeric(scalebar.length) || length(scalebar.length) != 1 || scalebar.length <= 0) {
      stop("scalebar.length must be a positive numeric value.")
    }
  }

  plot_xlim <- range(df$x, na.rm = TRUE)
  plot_ylim <- range(df$y, na.rm = TRUE)
  
  # 1. Define observation window (rectangle; replace with polygon mask if you have tissue boundary)
  win <- owin(range(df$x), range(df$y))
  
  # 2. Point pattern with marks = expression
  pp <- ppp(df$x, df$y, window = win, marks = df$expr)
  
  # 3. Choose bandwidth (sigma). Automatic selectors often oversmooth / undersmooth for sparse genes.
    # or set manually, e.g. sigma <- 40  (in your spatial units)
  sigma <- sigma %||% bw.diggle(pp)
  
  # 4a. Local transcript *sum* surface (kernel-weighted)
  sum_surface <- density(pp, weights = pp$marks, sigma = sigma, at = "pixels", edge=TRUE)
  # units: (sum of expression) / area
  
  df_sum  <- as.data.frame(sum_surface)  # columns: x, y, value
  
  # only plot the top n% of the values
  thr <- quantile(df_sum$value, threshold, na.rm=TRUE)
  maxv <- max(df_sum$value, na.rm=TRUE)
  breaks <- seq(thr, maxv, length.out = n_levels + 1)
  
  g <- ggplot() + theme_void()
  
  if (plot.all){
    if (is.null(group.by)){
      g <- g + geom_point(data = df, aes(x, y), color="grey", size=size, alpha = alpha)
    }else{
      g <- g + geom_point(data = df, aes(x, y, col = group.by), size=size, alpha = alpha) + NoLegend()
      if (!is.null(cols)){
        cols <- manual_scale_values(cols, df$group.by)
        g <- g + scale_color_manual(values = cols, drop = FALSE)
      }
    }
  }
  
  g <- g + geom_contour_filled(data = df_sum, aes(x=x, y=y, z = value), alpha = contour.alpha, breaks = breaks)

  if (dark.background){
    g <- g + theme(panel.background = element_rect(fill = "black", color = "black"))
    color_palette = color_palette %||% c("#3B0047","#D700FF")
  }else{
    color_palette = color_palette %||% c("#F7E7E6","red")
  }
  g <- g + scale_fill_manual(
    values = colorRampPalette(color_palette)(length(breaks) - 1),
    name = paste0("value ≥ ", signif(thr,3)),
    drop = FALSE)

  if (!is.null(scalebar.length)) {
    span_x <- diff(plot_xlim)
    span_y <- diff(plot_ylim)
    margin <- max(scalebar.margin, 0)
    margin_x <- span_x * margin
    margin_y <- span_y * margin
    offset_y <- if (span_y > 0) span_y * 0.02 else span_x * 0.02
    bar_colour <- if (is.null(scalebar.color)) {
      if (isTRUE(dark.background)) "white" else "black"
    } else {
      scalebar.color
    }

    if (is.character(scalebar.position)) {
      pos <- match.arg(scalebar.position, c("bottomright", "bottomleft", "topright", "topleft"))
      if (scalebar.length > span_x) {
        warning("scalebar.length exceeds the x-range of the plot and may be clipped.", call. = FALSE)
      }

      from_left <- grepl("left", pos)
      from_bottom <- grepl("bottom", pos)

      x_start <- if (from_left) plot_xlim[1] + margin_x else plot_xlim[2] - margin_x - scalebar.length
      y_start <- if (from_bottom) plot_ylim[1] + margin_y else plot_ylim[2] - margin_y
      label_y <- if (from_bottom) y_start + offset_y else y_start - offset_y
      text_vjust <- if (from_bottom) 0 else 1
    } else if (is.numeric(scalebar.position) && length(scalebar.position) == 2) {
      x_start <- scalebar.position[1]
      y_start <- scalebar.position[2]
      label_y <- y_start + offset_y
      text_vjust <- 0
    } else {
      stop("scalebar.position must be 'bottomright', 'bottomleft', 'topright', 'topleft', or a numeric length-2 vector.")
    }

    x_end <- x_start + scalebar.length
    scalebar.label <- round(scalebar.length * scalebar.numConv, digits = 2)
    label <- if (is.null(scalebar.unit) || scalebar.unit == "") {
      scalebar.label
    } else {
      paste(scalebar.label, scalebar.unit)
    }

    g <- g +
      annotate("segment", x = x_start, xend = x_end, y = y_start, yend = y_start, colour = bar_colour, linewidth = 0.5) +
      annotate("text", x = (x_start + x_end) / 2, y = label_y, label = label, colour = bar_colour, size = scalebar.text.size, vjust = text_vjust)
  }
  
  g <- g + coord_equal() # coord_fixed()
  
  return(g)
  
}







#' 
#'
#' @description Boxplot to visualize NULISA data, with t-test supported. 
#'
#' @details NULISA has different sensitivity to each protein, values below which is meaningless. 
#' Level of detection (LOD) is determined by the company. 
#' Typically, mark the samples below LOD, and exclude them from t-test.
#' 
#' @param nls A table of NULISA output, with additional metadata column if needed.
#' @param gene The gene of interest.
#' @param group.by The variable to group the data by.
#' @param group.color Optional. Colors for jitter points and boxplot. A named vector with colors as value and group elements as name.
#' @param split.by Optional. The variable to split the data by.
#' @param split.color Optional. Colors for split panel. A named vector with colors as value and split elements as name.
#' @param lod.value The limit of detection value. Use the value in table if NULL.
#' @param lod.mark Logical, indicating whether to mark values below LOD.
#' @param lod.replaceValue Optional. The value to replace values below LOD with, or "lod" to replace by LOD value.
#' @param lod.color The color for samples with value below LOD.
#' @param lod.line Logical, indicating whether to plot a line for LOD.
#' @param warn.mark Logical, indicating whether to mark values with WARN in SampleQC column.
#' @param warn.shape The shape for samples with WARN in SampleQC column.
#' @param ttest Logical, indicating whether to perform t-tests.
#' @param ttest.ignoreLOD Logical, indicating whether to ignore LOD in t-tests.
#' @param ttest.ignoreWARN Logical, indicating whether to ignore values with WARN sign in t-tests.
#' @param ttest.ignoreSplit Logical, indicating whether to perform t-test in each split panel or for all comparisons
#' @param plot.jitter Logical, indicating whether to plot jitter points.
#' @param jitter.size The size of jitter points.
#' @param jitter.stroke The thickness of jitter point borders. NA for no stroke.
#' @param jitter.width The width of jitter points.
#' @param plot.boxplot Logical, indicating whether to plot boxplot.
#' @param boxplot.linewidth The width of lines in boxplot.
#' @param plot.mean Logical, indicating whether to plot means.
#' @param mean.ignoreLOD Logical, indicating whether to ignore LOD in calculating the mean.
#' @param mean.ignoreWARN Logical, indicating whether to ignore values with WARN sign in calculating the mean
#' @param mean.linewidth The width of lines for mean indicators.
#' @param mean.color The color of mean indicators.
#' @param mean.fatten The fatten factor for mean indicators.
#' @param title The title of the plot.
#' @param alpha The transparency level of elements.
#' @param theme The theme to use for plotting.
#' @examples
#' BoxPlot.nulisa(nls=pd, gene="FGF23", group.by = "Skin", split.by = "Condition",
#' group.color=colors.skin, split.color=colors.disease)
#' BoxPlot.nulisa(nls=pd, gene="FGF23", group.by = "Skin", split.by = "Condition",
#'                group.color=colors.skin, ttest = T)
#' BoxPlot.nulisa(nls=pd[pd$Skin != "NonLesional",], gene="FGF23", group.by = "Condition",
#'                group.color=colors.disease, ttest = T)
#' @import scales ggplot2
#' @export
#'


BoxPlot.nulisa <- function(nls, gene, 
                           group.by, group.color=NULL,
                           split.by=NULL, split.color=NULL,
                           lod.value=NULL, lod.mark=T, lod.replaceValue=NULL, lod.color="grey", lod.line=T,
                           warn.mark=T, warn.shape=17,
                           ttest=T, ttest.ignoreLOD=T, ttest.ignoreWARN=T, ttest.ignoreSplit=F,
                           plot.jitter=T, jitter.size=1, jitter.stroke=0.2, jitter.width=0.2,
                           plot.boxplot=T, boxplot.linewidth=0.3,
                           plot.mean=F, mean.ignoreLOD=T, mean.ignoreWARN=T, mean.linewidth=0.6, mean.color="red",mean.fatten=0.5,
                           title="", alpha = 0.8, theme="bw"){
  # subset data
  df <- nls[nls$Target == gene,]
  
  # manage LOD
  df$LOD[is.na(df$LOD)] <- 0
  if (is.null(lod.value)) {
    lod.value <- unique(df$LOD)
    if (length(lod.value) > 1){
      warning("More than one LOD value for the gene.")
    }
  }
  df$lod.group <- ifelse(df$NPQ > df$LOD, "above", "below")
  df$valueToPlot <- df$NPQ
  if (!is.null(lod.replaceValue)){
    if (lod.replaceValue == "lod"){
      df$valueToPlot[df$lod.group == "below"] <- lod.value
    }else{
      df$valueToPlot[df$lod.group == "below"] <- lod.replaceValue
    }
  }
  
  # calculate mean
  if (plot.mean){
    df$valueForMean <- df$valueToPlot
    if (mean.ignoreLOD){
      df$valueForMean[df$lod.group == "below"] <- NA
    }
    if (mean.ignoreWARN){
      df$valueForMean[df$SampleQC == "WARN"] <- NA
    }
    
    if (is.null(split.by)){
      df.mean <- df %>% group_by(.data[[group.by]]) %>%
        summarise_at("valueForMean", .funs = "mean", na.rm=T)
    }else{
      df.mean <- df %>% group_by(.data[[group.by]], .data[[split.by]]) %>%
        summarise_at("valueForMean", .funs = "mean", na.rm=T)
    }
  }
  
  # plot
  g <- ggplot()
  
  if (is.null(warn.shape)){
    shape.by <- NULL
  }else{
    shape.by <- "SampleQC"
  }
  
  df$boxplot_group <- "y"
  df$color_group <- df[[group.by]]
  if (lod.mark){
    levels(df$color_group) <- c(levels(df$color_group), "below")
    df$color_group[df$lod.group == "below"] <- "below"
    df$boxplot_group[df$lod.group == "below"] <- "n"
  }
  if (plot.jitter){
    g <- g + geom_jitter(data = df, 
                         mapping = aes(x=.data[[group.by]], y=valueToPlot, 
                                       fill=color_group, shape=.data[[shape.by]]), 
                         size=jitter.size, alpha=1, height=0,stroke=jitter.stroke,colour="black", 
                         shape = 21, width = jitter.width)
  }
  if (plot.boxplot){
    g <- g + geom_boxplot(data = df[df$boxplot_group == "y",], 
                          mapping = aes(x=.data[[group.by]], y=valueToPlot, fill=.data[[group.by]]), 
                          linewidth=boxplot.linewidth, alpha=alpha, color="black",outlier.shape = NA)
  }
  if (!(is.null(warn.shape))){
    g <- g + scale_shape_manual(values = c('PASS' = 19, "WARN" = warn.shape))
  }
  
  if (plot.mean){
    g <- g + geom_crossbar(data = df.mean, mapping = aes(x=.data[[group.by]], y=valueForMean, ymin = valueForMean, ymax = valueForMean), width=mean.linewidth, color=mean.color, fatten = mean.fatten)
  }
  if (lod.line){
    g <- g + geom_hline(yintercept = lod.value, linetype="dashed", color="grey")
  }
  if (!is.null(split.by)){
    g <- g + facet_grid(reformulate(split.by), space = "free", scales = "free")
  }
  
  if (is.null(group.color)){
    group.color <- hue_pal()(length(levels(df[[group.by]])))
    names(group.color) <- levels(df[[group.by]])
  }
  
  group.color <- c(group.color, "below" = lod.color)
  group.color <- manual_scale_values(group.color, df$color_group)
  g <- g + scale_fill_manual(values = group.color) +
    scale_color_manual(values = group.color)
  
  if (!is.null(title)){
    g <- g + ggtitle(title)
  }
  if (!is.null(theme)){
    g <- g + do.call(paste0("theme_",theme), args = list())
  }
  
  if (!is.null(split.color)){
    # cannot do t-test when coloring split panel
    g2 <- ggplot_gtable(ggplot_build(g))
    stripr <- which(grepl('strip-t', g2$layout$name))
    fills <- c(split.color)
    k <- 1
    for (i in stripr) {
      j <- which(grepl('rect', g2$grobs[[i]]$grobs[[1]]$childrenOrder))
      g2$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
      k <- k+1
    }
    return(grid::grid.draw(g2))
  }
  
  
  
  
  if (ttest){
    df.ttest <- df
    if (ttest.ignoreLOD){
      df.ttest <- df.ttest[df.ttest$lod.group == "above",]
    }
    if (ttest.ignoreWARN){
      df.ttest <- df.ttest[df.ttest$SampleQC == "PASS",]
    }
    
    list.ttest <- list()
    if (is.null(split.by)){
      #  t-test all
      list.ttest[[1]] <- df.ttest
    }else{
      # plot split in multiple panels. Do t-test across groups with in each panel
      list.ttest <- split(df.ttest, df.ttest[,split.by])
    }
    
    ttestresult <- lapply(list.ttest, function(tb){
      groups <- unique(as.character(tb[,group.by]))
      if (length(groups) == 1){
        return(NULL)
      }
      comparisons <- as.data.frame(t(combn(groups,2)))
      colnames(comparisons) <- c("group1","group2")
      t.res <- data.frame()
      for (i in 1:nrow(comparisons)){
        tb2 <- split(tb, as.character(tb[,group.by]))
        if (nrow(tb2[[1]]) == 1 | nrow(tb2[[2]]) == 1) next
        # if (link){
        #   # if link is true, then do paired t-test with connected points.
        #   pointpaired <- intersect(tb2[[1]][,point.by], tb2[[2]][,point.by])
        #   if (length(pointpaired) < 2) message("Less than 3 points paired")
        #   tb2[[1]] <- tb2[[1]][match(pointpaired, tb2[[1]][,point.by]),]
        #   tb2[[2]] <- tb2[[2]][match(pointpaired, tb2[[2]][,point.by]),]
        #   tmp <- t.test(x=tb2[[comparisons[i,1]]][,"gene"], y=tb2[[comparisons[i,2]]][,"gene"], paired = T)
        #   
        # }else{
        # do with all samples with regular t-test
        tmp <- t.test(x=tb2[[comparisons[i,1]]][,"NPQ"], y=tb2[[comparisons[i,2]]][,"NPQ"], paired = F)
        # }
        t.res <- rbind(t.res,
                       data.frame(t = tmp$statistic,
                                  df = tmp$parameter,
                                  p = tmp$p.value,
                                  confint.low = tmp$conf.int[1],
                                  confint.high = tmp$conf.int[2],
                                  mean.1 = tmp$estimate[1],
                                  mean.2 = tmp$estimate[2],
                                  stderr = tmp$stderr))
      }
      if (nrow(t.res) > 0){
        res <- cbind(comparisons, t.res)
        return(res)
      }else{
        return(NULL)
      }
      
    })
    g <- g + ylab("NPQ") + ggtitle(gene) + theme(axis.title.x = element_blank(),
                                                 plot.title = element_text(hjust = 0.5))
    output <- list(g=g, ttest=ttestresult)
    return(output)
  }
  
  g <- g + ylab("NPQ") + ggtitle(gene) + theme(axis.title.x = element_blank(),
                                               plot.title = element_text(hjust = 0.5))
  return(g)
  
}

        apply_split_colors <- function(plot_obj, split_colors, split_values = NULL) {
            if (is.null(split_colors)) return(plot_obj)
            if (!is.null(split_values) && !is.null(names(split_colors))) {
                split_levels <- if (is.factor(split_values)) levels(split_values) else unique(as.character(split_values))
                ordered_colors <- split_colors[split_levels]
                if (any(!is.na(ordered_colors))) {
                    split_colors <- ordered_colors
                }
            }
            gtable_obj <- ggplot_gtable(ggplot_build(plot_obj))
            stripr <- which(grepl("strip-t", gtable_obj$layout$name))
            if (length(stripr) == 0) return(gtable_obj)
            fills <- c(split_colors)
            k <- 1
            for (i in stripr) {
                j <- which(grepl("rect", gtable_obj$grobs[[i]]$grobs[[1]]$childrenOrder))
                gtable_obj$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
                k <- k + 1
            }
            gtable_obj
        }

        resolve_color <- function(color_map, by) {
            if (is.null(color_map) || is.null(by)) return(NULL)
            if (is.list(color_map)) {
                if (!by %in% names(color_map)) return(NULL)
                return(color_map[[by]])
            }
            color_map
        }













# the order of df and meta should be the same
bulk_boxplot_plot <- function(cpm,meta,gene,group.by,split.by = NULL,shape.by = NULL,
                              group.color = NULL,
                              log2_scale = FALSE,ylab = "CPM") {

    sample_idx <- match(as.character(rownames(meta)), colnames(cpm))
    gene_vals <- cpm[gene, sample_idx, drop = FALSE]
    gene_vals <- suppressWarnings(as.numeric(as.matrix(gene_vals)))
    meta$gene <- gene_vals

    df <- meta[, c(group.by, split.by, shape.by, "gene"), drop = FALSE]

    df$plot_value <- if (isTRUE(log2_scale)) log2(df[["gene"]] + 1) else df[["gene"]]
    plot_ylab <- if (isTRUE(log2_scale)) paste0("log2(", ylab, " + 1)") else ylab

    g <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[group.by]], y = .data[["plot_value"]]))
    g <- g + ggplot2::geom_boxplot(
        ggplot2::aes(fill = .data[[group.by]]),
        outlier.shape = NA,
        linewidth = 0.3,
        alpha = 0.6
    )

    use_shape <- !is.null(shape.by) && shape.by %in% colnames(df)
    if (use_shape) {
        n_shapes <- length(unique(stats::na.omit(df[[shape.by]])))
        use_shape <- n_shapes <= 6
    }

    point_aes <- ggplot2::aes(color = .data[[group.by]])
    if (use_shape) {
        point_aes <- ggplot2::aes(color = .data[[group.by]], shape = .data[[shape.by]])
    }

    g <- g + ggplot2::geom_point(
        mapping = point_aes,
        position = ggplot2::position_jitter(width = 0.2, height = 0),
        size = 1.6
    )

    if (!is.null(group.color)) {
        g <- g + ggplot2::scale_color_manual(values = group.color) +
            ggplot2::scale_fill_manual(values = group.color)
    }

    if (!is.null(split.by) && split.by %in% colnames(df)) {
        g <- g + ggplot2::facet_wrap(stats::reformulate(split.by), scales = "free_y")
    }

    g + ggplot2::theme_classic() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "none",
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(face = "bold")
        ) +
        ggplot2::labs(x = NULL, y = plot_ylab) +
        ggplot2::guides(fill = "none")
}

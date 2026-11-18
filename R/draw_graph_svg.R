#' Draw a layered directed graph as a standalone SVG (base R only)
#'
#' Generic renderer for the family of repo/app diagram functions. It does its
#' own simple left-to-right layered layout, so no graph or plotting packages
#' are needed.
#'
#' @param nodes data.frame with columns `id`, `label`, `type`, and optionally
#'   `tooltip`. `type` is one of `"script"`, `"function"`, `"data"` (other
#'   values are drawn in grey).
#' @param edges data.frame with columns `from`, `to` (node ids) and `type`
#'   (one of `"define"`, `"call"`, `"use"`, `"produce"`).
#' @param file Optional path. If given, the SVG is written there.
#' @param title Optional title drawn at the top.
#' @param right_types Node types forced into the right-most column.
#'
#' @return The SVG as a single string, invisibly.
#' @noRd
draw_graph_svg <- function(nodes, edges, file = NULL, title = NULL,
                           right_types = "data") {
  stopifnot(
    all(c("id", "label", "type") %in% names(nodes)),
    all(c("from", "to", "type") %in% names(edges)),
    !anyDuplicated(nodes$id),
    all(c(edges$from, edges$to) %in% nodes$id)
  )
  if (!"tooltip" %in% names(nodes)) nodes$tooltip <- nodes$label
  nodes <- nodes[order(nodes$type, nodes$label), , drop = FALSE]
  n <- nrow(nodes)
  ids <- nodes$id

  # ---- layout ---------------------------------------------------------------
  layer <- graph_layers(ids, edges$from, edges$to)
  is_right <- nodes$type %in% right_types
  if (any(is_right) && any(!is_right)) {
    layer[is_right] <- max(layer[!is_right]) + 1L
  }
  layer <- layer - min(layer)

  node_h  <- 22
  row_gap <- 8
  col_gap <- 150
  margin  <- 20
  top     <- if (is.null(title)) 60 else 85

  w <- nchar(nodes$label) * 6.6 + 18
  rank <- order_within_layers(ids, layer, edges$from, edges$to)

  layers <- sort(unique(layer))
  col_w  <- vapply(layers, function(l) max(w[layer == l]), numeric(1))
  col_x  <- margin + c(0, cumsum(col_w + col_gap))[seq_along(layers)]
  x <- col_x[match(layer, layers)]

  layer_n <- vapply(layers, function(l) sum(layer == l), numeric(1))
  row_h   <- node_h + row_gap
  H       <- max(layer_n) * row_h
  y <- top + (H - layer_n[match(layer, layers)] * row_h) / 2 +
    (rank - 1) * row_h

  total_w <- max(x + w) + margin
  total_h <- top + H + margin

  # ---- edges ----------------------------------------------------------------
  edge_style <- list(
    define  = list(col = "#9a9a9a", dash = "4 3"),
    call    = list(col = "#4a6fa5", dash = ""),
    use     = list(col = "#d98324", dash = ""),
    produce = list(col = "#b5651d", dash = "2 3")
  )
  fallback <- list(col = "#777777", dash = "")
  style_of <- function(t) if (!is.null(edge_style[[t]])) edge_style[[t]] else fallback

  fi <- match(edges$from, ids)
  ti <- match(edges$to, ids)
  x1 <- x[fi] + w[fi]
  y1 <- y[fi] + node_h / 2
  x2 <- x[ti]
  y2 <- y[ti] + node_h / 2
  dx <- pmax(40, abs(x2 - x1) / 2)

  edge_svg <- vapply(seq_len(nrow(edges)), function(i) {
    s <- style_of(edges$type[i])
    sprintf(
      paste0('<path class="edge" d="M%.1f %.1f C%.1f %.1f %.1f %.1f %.1f %.1f" ',
             'fill="none" stroke="%s" stroke-opacity="0.45" stroke-width="1.1" ',
             '%s marker-end="url(#arrow-%s)"><title>%s</title></path>'),
      x1[i], y1[i], x1[i] + dx[i], y1[i], x2[i] - dx[i], y2[i], x2[i], y2[i],
      s$col,
      if (nzchar(s$dash)) sprintf('stroke-dasharray="%s"', s$dash) else "",
      if (is.null(edge_style[[edges$type[i]]])) "other" else edges$type[i],
      svg_escape(sprintf("%s -> %s (%s)", nodes$label[fi[i]],
                         nodes$label[ti[i]], edges$type[i]))
    )
  }, character(1))

  # ---- nodes ----------------------------------------------------------------
  node_style <- list(
    script   = list(fill = "#d6e6f7", stroke = "#4a6fa5", rx = 3),
    "function" = list(fill = "#dff0d8", stroke = "#5a9a4a", rx = 11),
    data     = list(fill = "#fbe5c8", stroke = "#d98324", rx = 3)
  )
  nfallback <- list(fill = "#eeeeee", stroke = "#888888", rx = 3)
  node_svg <- vapply(seq_len(n), function(i) {
    s <- node_style[[nodes$type[i]]]
    if (is.null(s)) s <- nfallback
    sprintf(
      paste0('<g class="node"><title>%s</title>',
             '<rect x="%.1f" y="%.1f" width="%.1f" height="%d" rx="%d" ',
             'fill="%s" stroke="%s"/>',
             '<text x="%.1f" y="%.1f" text-anchor="middle" ',
             'dominant-baseline="central">%s</text></g>'),
      svg_escape(nodes$tooltip[i]),
      x[i], y[i], w[i], node_h, s$rx, s$fill, s$stroke,
      x[i] + w[i] / 2, y[i] + node_h / 2, svg_escape(nodes$label[i])
    )
  }, character(1))

  # ---- legend / title / defs ------------------------------------------------
  markers <- vapply(c(names(edge_style), "other"), function(t) {
    col <- if (t == "other") fallback$col else edge_style[[t]]$col
    sprintf(paste0('<marker id="arrow-%s" viewBox="0 0 10 10" refX="9" refY="5" ',
                   'markerWidth="6" markerHeight="6" orient="auto-start-reverse">',
                   '<path d="M0 0 L10 5 L0 10 z" fill="%s"/></marker>'), t, col)
  }, character(1))

  legend_items <- c(
    sprintf('<rect x="%d" y="%d" width="26" height="14" rx="3" fill="%s" stroke="%s"/><text x="%d" y="%d">script</text>',
            margin, 18, node_style$script$fill, node_style$script$stroke, margin + 32, 25),
    sprintf('<rect x="%d" y="%d" width="26" height="14" rx="7" fill="%s" stroke="%s"/><text x="%d" y="%d">function</text>',
            margin + 100, 18, node_style[["function"]]$fill, node_style[["function"]]$stroke, margin + 132, 25),
    sprintf('<rect x="%d" y="%d" width="26" height="14" rx="3" fill="%s" stroke="%s"/><text x="%d" y="%d">data</text>',
            margin + 215, 18, node_style$data$fill, node_style$data$stroke, margin + 247, 25)
  )
  edge_labels <- c(define = "defines", call = "calls", use = "uses data",
                   produce = "produces data")
  ex <- margin + 330
  for (t in names(edge_labels)) {
    s <- edge_style[[t]]
    legend_items <- c(legend_items, sprintf(
      '<line x1="%.0f" y1="25" x2="%.0f" y2="25" stroke="%s" stroke-width="1.6" %s/><text x="%.0f" y="25">%s</text>',
      ex, ex + 28, s$col,
      if (nzchar(s$dash)) sprintf('stroke-dasharray="%s"', s$dash) else "",
      ex + 34, edge_labels[[t]]
    ))
    ex <- ex + 34 + nchar(edge_labels[[t]]) * 6.6 + 24
  }
  total_w <- max(total_w, ex + margin)

  title_svg <- if (is.null(title)) "" else sprintf(
    '<text x="%d" y="62" font-size="15" font-weight="bold">%s</text>',
    margin, svg_escape(title)
  )

  svg <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f" font-family="Helvetica, Arial, sans-serif" font-size="11">\n',
            total_w, total_h, total_w, total_h),
    '<style>.edge:hover{stroke-opacity:1;stroke-width:2.4}.node:hover rect{stroke-width:2.5}.node text{pointer-events:none}text{dominant-baseline:central}</style>\n',
    '<defs>', paste(markers, collapse = ""), '</defs>\n',
    '<rect width="100%" height="100%" fill="white"/>\n',
    '<g class="legend">', paste(legend_items, collapse = ""), '</g>\n',
    title_svg, '\n',
    '<g class="edges">', paste(edge_svg, collapse = "\n"), '</g>\n',
    '<g class="nodes">', paste(node_svg, collapse = "\n"), '</g>\n',
    '</svg>\n'
  )

  if (!is.null(file)) writeLines(svg, file, useBytes = TRUE)
  invisible(svg)
}

# Helpers -----------------------------------------------------------------

svg_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

# Longest-path layering. Back edges found by DFS are ignored so cycles
# (e.g. recursive functions) don't blow up the layer count.
graph_layers <- function(ids, from, to) {
  keep <- from != to
  from <- from[keep]
  to   <- to[keep]

  state <- stats_free_zeros(ids)
  is_back <- logical(length(from))
  visit <- function(v) {
    state[v] <<- 1L
    for (i in which(from == v)) {
      w <- to[i]
      if (state[w] == 1L) {
        is_back[i] <<- TRUE
      } else if (state[w] == 0L) {
        visit(w)
      }
    }
    state[v] <<- 2L
  }
  for (v in ids) if (state[v] == 0L) visit(v)

  from <- from[!is_back]
  to   <- to[!is_back]

  layer <- stats_free_zeros(ids)
  repeat {
    new <- layer
    for (i in seq_along(from)) {
      new[to[i]] <- max(new[to[i]], new[from[i]] + 1L)
    }
    if (identical(new, layer)) break
    layer <- new
  }
  layer
}

stats_free_zeros <- function(ids) {
  out <- integer(length(ids))
  names(out) <- ids
  out
}

# Barycentre ordering to reduce edge crossings; returns 1-based rank within
# each node's layer (same order as `ids`).
order_within_layers <- function(ids, layer, from, to) {
  n <- length(ids)
  layer <- unname(layer)
  rank <- numeric(n)
  for (l in unique(layer)) {
    idx <- which(layer == l)
    rank[idx] <- seq_along(idx)
  }
  size <- vapply(layer, function(l) sum(layer == l), numeric(1))
  fi <- match(from, ids)
  ti <- match(to, ids)
  nbrs <- lapply(seq_len(n), function(i) c(ti[fi == i], fi[ti == i]))

  layers <- sort(unique(layer))
  for (sweep in 1:8) {
    for (l in if (sweep %% 2) layers else rev(layers)) {
      idx <- which(layer == l)
      pos <- rank / size
      bary <- vapply(idx, function(i) {
        nb <- nbrs[[i]]
        if (length(nb)) mean(pos[nb]) else pos[i]
      }, numeric(1))
      rank[idx[order(bary, rank[idx])]] <- seq_along(idx)
    }
  }
  rank
}

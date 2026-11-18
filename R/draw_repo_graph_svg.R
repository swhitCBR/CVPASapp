#' Find scripts, functions, and data objects in the repo and how they connect
#'
#' Uses only base R. Scripts are parsed (not run); references are detected by
#' matching names found in the parsed code against the set of top-level
#' functions and `data/*.rda` objects. This is name-based, so a local variable
#' that happens to share a name with a function or dataset will produce a
#' false edge.
#'
#' Edge types: `define` (script -> function), `call` (function/script ->
#' function), `use` (function/script -> data), `produce` (data-raw script ->
#' data via `use_data()`).
#'
#' @param root Repo root.
#' @param dirs Directories or files (relative to `root`) to scan for R code.
#' @param fold_single_fun_scripts If `TRUE`, a script that defines exactly one
#'   function and has no other top-level code is represented by that function
#'   node alone (the file name is kept in the tooltip), which removes many
#'   redundant script/function pairs.
#'
#' @return A list with data.frames `nodes` and `edges`.
#' @noRd
get_repo_graph_edges <- function(root = ".",
                                 dirs = c("R", "data-raw", "inst", "app.R"),
                                 fold_single_fun_scripts = TRUE) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  paths <- unlist(lapply(file.path(root, dirs), function(p) {
    if (dir.exists(p)) {
      list.files(p, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
    } else if (file.exists(p)) {
      p
    }
  }))
  rel <- substring(normalizePath(paths, winslash = "/"), nchar(root) + 2)

  data_files <- list.files(file.path(root, "data"),
                           pattern = "\\.(rda|RData|rdata)$")
  data_names <- sub("\\.[^.]*$", "", data_files)

  parsed <- lapply(seq_along(paths), function(i) {
    tryCatch(
      as.list(parse(paths[i], keep.source = FALSE, encoding = "UTF-8")),
      error = function(e) {
        warning("Could not parse ", rel[i], ": ", conditionMessage(e),
                call. = FALSE)
        list()
      }
    )
  })

  # Split each script into function definitions and other top-level code
  scripts <- lapply(parsed, function(exprs) {
    is_def <- vapply(exprs, is_fun_def, logical(1))
    defs <- exprs[is_def]
    names(defs) <- vapply(defs, function(e) as.character(e[[2]]), character(1))
    list(defs = defs, other = exprs[!is_def])
  })

  # First definition wins if a name is defined in more than one file
  def_files <- rep(rel, vapply(scripts, function(s) length(s$defs), integer(1)))
  def_names <- unlist(lapply(scripts, function(s) names(s$defs)))
  dup <- duplicated(def_names)
  if (any(dup)) {
    message("Functions defined in more than one file (first kept): ",
            paste(unique(def_names[dup]), collapse = ", "))
  }
  fun_names <- unique(def_names)

  script_id <- paste0("script:", rel)
  folded <- fold_single_fun_scripts &
    vapply(scripts, function(s) length(s$defs) == 1L && !length(s$other),
         logical(1))
  owner <- ifelse(
    folded,
    paste0("fun:", vapply(scripts, function(s) names(s$defs)[1] %||% "", "")),
    script_id
  )

  edge_list <- list()
  # `to` is given as bare names plus a prefix, because paste0() on a
  # zero-length vector returns the bare prefix rather than character(0)
  add_edges <- function(from, to, type, prefix = "") {
    if (length(to)) {
      edge_list[[length(edge_list) + 1L]] <<- data.frame(
        from = from, to = paste0(prefix, to), type = type,
        stringsAsFactors = FALSE
      )
    }
  }

  # Scripts referenced by file name in a string, e.g.
  # source(system.file("global.R", package = "CVPASapp")), become call edges
  script_targets <- function(e) {
    str <- find_strings(e)
    str <- str[grepl("\\.[Rr]$", str)]
    if (!length(str)) return(character())
    hit <- rel %in% str |
      vapply(rel, function(r) any(endsWith(r, paste0("/", str))), logical(1))
    unique(owner[hit])
  }

  for (i in seq_along(scripts)) {
    s <- scripts[[i]]

    if (!folded[i] && length(s$defs)) {
      add_edges(script_id[i], unique(names(s$defs)), "define", "fun:")
    }

    for (f in unique(names(s$defs))) {
      nm <- fun_all_names(s$defs[[f]][[3]])
      add_edges(paste0("fun:", f), setdiff(intersect(nm, fun_names), f),
                "call", "fun:")
      add_edges(paste0("fun:", f), script_targets(s$defs[[f]][[3]]), "call")
      add_edges(paste0("fun:", f), intersect(nm, data_names), "use", "data:")
    }

    # Top-level (non-function) code, e.g. data-raw and global scripts
    if (length(s$other)) {
      produced <- unique(unlist(lapply(s$other, function(e) {
        unlist(lapply(find_calls(e, "use_data"), function(cl) {
          all.names(cl[-1])
        }))
      })))
      produced <- intersect(produced, data_names)
      nm <- unique(unlist(lapply(s$other, all.names)))
      add_edges(owner[i], unlist(lapply(s$other, script_targets)), "call")
      add_edges(owner[i], produced, "produce", "data:")
      add_edges(owner[i], intersect(nm, fun_names), "call", "fun:")
      add_edges(owner[i], setdiff(intersect(nm, data_names), produced),
                "use", "data:")
    }
  }

  edges <- unique(do.call(rbind, edge_list))
  edges <- edges[edges$from != edges$to, , drop = FALSE]
  rownames(edges) <- NULL

  # Nodes
  fun_nodes <- data.frame(
    id = paste0("fun:", fun_names),
    label = fun_names,
    type = "function",
    tooltip = paste0(fun_names, "()  --  ", def_files[match(fun_names, def_names)]),
    stringsAsFactors = FALSE
  )
  script_nodes <- data.frame(
    id = script_id[!folded],
    label = rel[!folded],
    type = "script",
    tooltip = rel[!folded],
    stringsAsFactors = FALSE
  )
  data_nodes <- data.frame(
    id = paste0("data:", data_names),
    label = data_names,
    type = "data",
    tooltip = file.path("data", data_files),
    stringsAsFactors = FALSE
  )
  nodes <- rbind(script_nodes, fun_nodes, data_nodes)
  nodes <- nodes[!duplicated(nodes$id), , drop = FALSE]
  rownames(nodes) <- NULL

  list(nodes = nodes, edges = edges)
}

#' Draw an SVG of the scripts, functions, and data connected to the app
#'
#' Shows only what is connected to `app_server()` or `app_ui()` (see
#' [get_connected_ids()] for the exact definition of "connected").
#'
#' @inheritParams get_repo_graph_edges
#' @param file Output path for the SVG. `NULL` returns the SVG string only.
#' @param roots Names of the app entry-point functions.
#' @param drop_isolated Drop nodes that have no edges (after `edge_types`
#'   filtering).
#' @param edge_types Which edge types to draw. Connectivity is always
#'   computed from all edges; this only affects what is drawn. Dropping
#'   `"call"` gives a much sparser view of just where data is touched.
#'
#' @return The SVG string, invisibly.
#' @noRd
draw_repo_graph_svg <- function(root = ".",
                                file = NULL,
                                roots = c("app_server", "app_ui"),
                                dirs = c("R", "data-raw", "inst", "app.R"),
                                fold_single_fun_scripts = TRUE,
                                drop_isolated = TRUE,
                                edge_types = c("define", "call", "use", "produce")) {
  g <- get_repo_graph_edges(root, dirs, fold_single_fun_scripts)
  keep <- get_connected_ids(g$nodes, g$edges, roots)
  draw_repo_subgraph_svg(
    g, keep, file = file, drop_isolated = drop_isolated,
    edge_types = edge_types,
    title = paste0("Connected to ", paste0(roots, "()", collapse = " / "))
  )
}

#' Draw an SVG of the scripts, functions, and data NOT connected to the app
#'
#' The complement of [draw_repo_graph_svg()]: everything found in the repo
#' that is not connected to `app_server()` or `app_ui()`. Candidates for
#' dead code, one-off dev/data-prep scripts, and unused datasets. Nodes with
#' no edges are kept by default, since an unused function usually has none.
#'
#' @inheritParams draw_repo_graph_svg
#'
#' @return The SVG string, invisibly.
#' @noRd
find_disconnected_graph <- function(root = ".",
                                    file = NULL,
                                    roots = c("app_server", "app_ui"),
                                    dirs = c("R", "data-raw", "inst", "app.R"),
                                    fold_single_fun_scripts = TRUE,
                                    drop_isolated = FALSE,
                                    edge_types = c("define", "call", "use", "produce")) {
  g <- get_repo_graph_edges(root, dirs, fold_single_fun_scripts)
  keep <- setdiff(g$nodes$id, get_connected_ids(g$nodes, g$edges, roots))
  draw_repo_subgraph_svg(
    g, keep, file = file, drop_isolated = drop_isolated,
    edge_types = edge_types,
    title = paste0("NOT connected to ", paste0(roots, "()", collapse = " / "))
  )
}

#' Ids of nodes connected to the app entry points
#'
#' Connected means any of:
#' * a root, or a transitive caller of a root (e.g. `run_app()`, `app.R`);
#' * reachable from any of those by following `call`/`use` edges (what the
#'   app needs at run time, including files `run_app()` sources);
#' * a script that `define`s or `produce`s one of the above (e.g. the
#'   `data-raw` script that creates a dataset the app uses).
#'
#' @noRd
get_connected_ids <- function(nodes, edges, roots) {
  root_ids <- paste0("fun:", roots)
  absent <- setdiff(root_ids, nodes$id)
  if (length(absent)) {
    stop("Root function(s) not found in the repo: ",
         paste(sub("^fun:", "", absent), collapse = ", "), call. = FALSE)
  }

  walk <- function(start, from, to) {
    seen <- start
    repeat {
      nxt <- setdiff(to[from %in% seen], seen)
      if (!length(nxt)) break
      seen <- c(seen, nxt)
    }
    seen
  }
  flow <- edges[edges$type %in% c("call", "use"), , drop = FALSE]
  callers <- walk(root_ids, flow$to, flow$from)
  keep <- union(callers, walk(callers, flow$from, flow$to))
  parents <- edges[edges$type %in% c("define", "produce") & edges$to %in% keep, ]
  union(keep, parents$from)
}

draw_repo_subgraph_svg <- function(g, keep_ids, file, drop_isolated,
                                   edge_types, title) {
  edges <- g$edges[g$edges$from %in% keep_ids & g$edges$to %in% keep_ids &
                     g$edges$type %in% edge_types, , drop = FALSE]
  nodes <- g$nodes[g$nodes$id %in% keep_ids, , drop = FALSE]
  if (drop_isolated) {
    nodes <- nodes[nodes$id %in% c(edges$from, edges$to), , drop = FALSE]
  }
  if (!nrow(nodes)) {
    message("No nodes to draw.")
    return(invisible(NULL))
  }
  draw_graph_svg(nodes, edges, file = file, title = title)
}

# Helpers -----------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

is_assign_call <- function(e) {
  is.call(e) && length(e) == 3L &&
    (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("=")))
}

is_fun_def <- function(e) {
  is_assign_call(e) && is.name(e[[2]]) && is.call(e[[3]]) &&
    identical(e[[3]][[1]], as.name("function"))
}

# All function/symbol names in a function definition, including formals'
# default values (which all.names() does not descend into)
fun_all_names <- function(def) {
  fm <- def[[2]]
  defaults <- lapply(seq_along(fm), function(i) {
    if (identical(fm[[i]], quote(expr = ))) NULL else fm[[i]]
  })
  unique(c(all.names(def[[3]]), unlist(lapply(defaults, all.names))))
}

# All string literals in an expression
find_strings <- function(e) {
  if (is.character(e)) return(e)
  if (!is.call(e)) return(character())
  out <- character()
  parts <- as.list(e)
  for (i in seq_along(parts)) {
    if (identical(parts[[i]], quote(expr = ))) next
    out <- c(out, find_strings(parts[[i]]))
  }
  out
}

# Recursively collect calls to `fn` (with or without a `pkg::` prefix)
find_calls <- function(e, fn) {
  if (!is.call(e)) return(list())
  head <- e[[1]]
  nm <- if (is.name(head)) {
    as.character(head)
  } else if (is.call(head) && length(head) == 3L &&
             as.character(head[[1]])[1] %in% c("::", ":::")) {
    as.character(head[[3]])
  } else {
    ""
  }
  out <- if (identical(nm, fn)) list(e) else list()
  parts <- as.list(e)
  for (i in seq_along(parts)) {
    # Empty arguments (e.g. x[, 1]) can't be evaluated, so skip them
    if (identical(parts[[i]], quote(expr = ))) next
    out <- c(out, find_calls(parts[[i]], fn))
  }
  out
}

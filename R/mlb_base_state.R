# Per-event base-state reconstruction for mlb_pbp() (#276).
#
# The MLB Stats API only publishes base state at the end of each plate
# appearance (matchup.postOn*). Each at-bat's runners[] array, however,
# records every base movement with details.playIndex tying it to the play
# event it occurred on -- so the pre-event state is reconstructable: carry
# state across at-bats within a half-inning, and before event e apply every
# movement with playIndex < e.
#
# Movement batches are applied vacate-first (all starts cleared before any
# end is set) so simultaneous advances (1B->2B + 2B->3B on one play) cannot
# clobber each other, and a runner taking multiple segments in one play
# (1B->2B->3B) keeps only the final base. Validated per-pitch against
# Statcast's own on_1b/on_2b/on_3b columns (see tests).

#' @keywords internal
#' @noRd
.mlb_base_state <- function(payload) {
  ab <- payload$liveData$plays$allPlays
  if (is.null(ab) || NROW(ab) == 0) {
    return(data.frame(atBatIndex = integer(), index = integer(),
                      pre_on_1b = integer(), pre_on_2b = integer(),
                      pre_on_3b = integer()))
  }

  bases <- c("1B" = NA_integer_, "2B" = NA_integer_, "3B" = NA_integer_)
  cur_half <- ""
  out <- vector("list", nrow(ab))

  apply_batch <- function(bases, mv) {
    if (nrow(mv) == 0) return(bases)
    # vacate every start first
    for (s in mv$start) {
      if (!is.na(s) && s %in% names(bases)) bases[[s]] <- NA_integer_
    }
    # keep each runner's final segment only
    last_seg <- !duplicated(mv$runner, fromLast = TRUE)
    for (j in which(last_seg)) {
      en <- mv$end[j]
      if (!is.na(en) && en %in% names(bases)) bases[[en]] <- mv$runner[j]
    }
    bases
  }

  for (i in seq_len(nrow(ab))) {
    half <- paste(ab$about.inning[i], ab$about.halfInning[i])
    if (!identical(half, cur_half)) {
      bases[] <- NA_integer_
      cur_half <- half
    }

    pe <- ab$playEvents[[i]]
    ev_idx <- if (is.data.frame(pe) && "index" %in% names(pe)) pe$index else integer()
    # The extra-innings automatic runner arrives as a "runner_placed" ACTION
    # event ("<name> starts inning at 2nd base."), not as a runners[]
    # movement -- capture it so the placed runner appears from the next
    # event onward.
    ev_type <- if (is.data.frame(pe) && "details.eventType" %in% names(pe)) {
      as.character(pe$details.eventType)
    } else rep(NA_character_, length(ev_idx))
    ev_player <- if (is.data.frame(pe) && "player.id" %in% names(pe)) {
      suppressWarnings(as.integer(pe$player.id))
    } else rep(NA_integer_, length(ev_idx))
    ev_desc <- if (is.data.frame(pe) && "details.description" %in% names(pe)) {
      as.character(pe$details.description)
    } else rep(NA_character_, length(ev_idx))

    r <- ab$runners[[i]]
    grab <- function(col, n) {
      if (is.data.frame(r) && col %in% names(r)) unlist(r[[col]]) else rep(NA, n)
    }
    n_mv <- if (is.data.frame(r)) nrow(r) else 0L
    mv <- data.frame(
      playIndex = suppressWarnings(as.integer(grab("details.playIndex", n_mv))),
      start = as.character(grab("movement.start", n_mv)),
      end = as.character(grab("movement.end", n_mv)),
      runner = suppressWarnings(as.integer(grab("details.runner.id", n_mv))),
      stringsAsFactors = FALSE
    )
    mv <- mv[!is.na(mv$playIndex), , drop = FALSE]
    mv <- mv[order(mv$playIndex), , drop = FALSE]

    rows <- matrix(NA_integer_, nrow = length(ev_idx), ncol = 5)
    for (k in seq_along(ev_idx)) {
      e <- ev_idx[k]
      batch <- mv[mv$playIndex < e, , drop = FALSE]
      if (nrow(batch) > 0) {
        bases <- apply_batch(bases, batch)
        mv <- mv[mv$playIndex >= e, , drop = FALSE]
      }
      rows[k, ] <- c(ab$atBatIndex[i], e, bases[["1B"]], bases[["2B"]], bases[["3B"]])

      if (identical(ev_type[k], "runner_placed") && !is.na(ev_player[k])) {
        placed_base <- if (grepl("3rd", ev_desc[k] %||% "")) "3B"
        else if (grepl("1st", ev_desc[k] %||% "")) "1B"
        else "2B"
        bases[[placed_base]] <- ev_player[k]
      }
    }
    # apply the at-bat's remaining movements so state carries into the next AB
    bases <- apply_batch(bases, mv)
    out[[i]] <- rows
  }

  res <- as.data.frame(do.call(rbind, out))
  names(res) <- c("atBatIndex", "index", "pre_on_1b", "pre_on_2b", "pre_on_3b")
  res
}

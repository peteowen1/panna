# Pitch Drawing and Chain Visualization
#
# Functions for drawing football pitches and visualizing possession chains.
# Uses ggplot2 with SPADL coordinate system (0-100 for both x and y).

#' Draw a Football Pitch
#'
#' Creates a ggplot2 object with a football pitch overlay using the SPADL
#' coordinate system (0-100 for both x and y axes).
#'
#' @param background Character. Background style: "white" (default) or "green".
#'
#' @return A ggplot2 object with pitch markings.
#'
#' @export
#' @examples
#' \dontrun{
#' draw_pitch()
#' draw_pitch("green")
#' }
draw_pitch <- function(background = c("white", "green")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for pitch visualization.")
  }

  background <- match.arg(background)

  bg_color <- if (background == "green") "#2e8b57" else "white"
  line_color <- if (background == "green") "white" else "black"

  # Pitch boundaries on 0-100 scale
  p <- ggplot2::ggplot() +
    ggplot2::coord_fixed(ratio = 68 / 105) +
    ggplot2::xlim(-2, 102) +
    ggplot2::ylim(-2, 102) +

    # Background
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA)
    ) +

    # Pitch outline
    ggplot2::annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100,
                      fill = NA, color = line_color, linewidth = 0.8) +

    # Halfway line
    ggplot2::annotate("segment", x = 50, xend = 50, y = 0, yend = 100,
                      color = line_color, linewidth = 0.5) +

    # Center circle (approximate with polygon)
    ggplot2::annotate("path",
                      x = 50 + 9.15 * cos(seq(0, 2 * pi, length.out = 100)),
                      y = 50 + 9.15 * sin(seq(0, 2 * pi, length.out = 100)),
                      color = line_color, linewidth = 0.5) +

    # Center spot
    ggplot2::annotate("point", x = 50, y = 50,
                      color = line_color, size = 1) +

    # Left penalty area (0-17, 21.1-78.9)
    ggplot2::annotate("rect", xmin = 0, xmax = 17, ymin = 21.1, ymax = 78.9,
                      fill = NA, color = line_color, linewidth = 0.5) +

    # Right penalty area (83-100, 21.1-78.9)
    ggplot2::annotate("rect", xmin = 83, xmax = 100, ymin = 21.1, ymax = 78.9,
                      fill = NA, color = line_color, linewidth = 0.5) +

    # Left six-yard box (0-6, 37-63)
    ggplot2::annotate("rect", xmin = 0, xmax = 6, ymin = 37, ymax = 63,
                      fill = NA, color = line_color, linewidth = 0.5) +

    # Right six-yard box (94-100, 37-63)
    ggplot2::annotate("rect", xmin = 94, xmax = 100, ymin = 37, ymax = 63,
                      fill = NA, color = line_color, linewidth = 0.5) +

    # Penalty spots
    ggplot2::annotate("point", x = c(11.5, 88.5), y = c(50, 50),
                      color = line_color, size = 1) +

    # Goal lines (as rectangles behind goal)
    ggplot2::annotate("rect", xmin = -2, xmax = 0, ymin = 45.2, ymax = 54.8,
                      fill = NA, color = line_color, linewidth = 0.5) +
    ggplot2::annotate("rect", xmin = 100, xmax = 102, ymin = 45.2, ymax = 54.8,
                      fill = NA, color = line_color, linewidth = 0.5)

  p
}


#' Plot a Single Possession Chain
#'
#' Draws a single chain's actions as connected arrows on a pitch, color-coded
#' by action type.
#'
#' @param spadl_with_chains SPADL actions with chain_id column.
#' @param target_match_id Character. Match ID to filter.
#' @param target_chain_id Integer. Chain ID within the match.
#' @param background Character. Pitch background: "white" (default) or "green".
#'
#' @return A ggplot2 object showing the chain on a pitch.
#'
#' @export
#' @examples
#' \dontrun{
#' spadl <- create_possession_chains(convert_opta_to_spadl(events))
#' plot_chain(spadl, match_id = "abc123", chain_id = 5)
#' }
plot_chain <- function(spadl_with_chains, target_match_id, target_chain_id,
                       background = c("white", "green")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for chain visualization.")
  }

  background <- match.arg(background)

  chain_data <- spadl_with_chains[
    spadl_with_chains$match_id == target_match_id &
    spadl_with_chains$chain_id == target_chain_id, ]

  if (nrow(chain_data) == 0) {
    cli::cli_abort("No actions found for match_id={target_match_id}, chain_id={target_chain_id}")
  }

  # Action type color mapping
  action_colors <- c(
    "pass" = "#3498db",
    "cross" = "#2980b9",
    "dribble" = "#27ae60",
    "shot" = "#e74c3c",
    "tackle" = "#e67e22",
    "interception" = "#f39c12",
    "clearance" = "#95a5a6",
    "foul" = "#8e44ad",
    "goalkick" = "#95a5a6",
    "throw_in" = "#95a5a6",
    "freekick_short" = "#2980b9",
    "freekick_crossed" = "#2980b9",
    "corner_short" = "#2980b9",
    "corner_crossed" = "#2980b9"
  )

  p <- draw_pitch(background)

  # Add arrows for each action
  p <- p + ggplot2::geom_segment(
    data = chain_data,
    ggplot2::aes(
      x = .data$start_x, y = .data$start_y,
      xend = .data$end_x, yend = .data$end_y,
      color = .data$action_type
    ),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
    linewidth = 0.8, alpha = 0.8
  )

  # Add start points
  p <- p + ggplot2::geom_point(
    data = chain_data,
    ggplot2::aes(x = .data$start_x, y = .data$start_y, color = .data$action_type),
    size = 2
  )

  # Label shots and goals
  shot_data <- chain_data[chain_data$action_type == "shot", ]
  if (nrow(shot_data) > 0) {
    shot_data$label <- ifelse(shot_data$result == "success", "GOAL", "Shot")
    p <- p + ggplot2::geom_label(
      data = shot_data,
      ggplot2::aes(x = .data$start_x, y = .data$start_y, label = .data$label),
      size = 3, fill = "#e74c3c", color = "white", fontface = "bold",
      nudge_y = 3
    )
  }

  # Apply color scale
  p <- p + ggplot2::scale_color_manual(
    values = action_colors,
    na.value = "#bdc3c7",
    name = "Action"
  ) +
    ggplot2::labs(
      title = paste0("Chain #", target_chain_id),
      subtitle = paste0(chain_data$team_id[1], " | ",
                        nrow(chain_data), " actions")
    )

  p
}


#' Plot All Chains for a Match
#'
#' Draws all possession chains for a match (or one team) as semi-transparent
#' paths on a pitch. Chains ending in goals are highlighted.
#'
#' @param spadl_with_chains SPADL actions with chain_id and optionally
#'   chain_outcome columns.
#' @param target_match_id Character. Match ID to filter.
#' @param target_team_id Character. Optional team ID to show only one team's chains.
#' @param highlight_goals Logical. Highlight chains ending in goals (default TRUE).
#' @param background Character. Pitch background: "white" (default) or "green".
#'
#' @return A ggplot2 object showing possession chains on a pitch.
#'
#' @export
#' @examples
#' \dontrun{
#' spadl <- create_possession_chains(convert_opta_to_spadl(events))
#' outcomes <- classify_chain_outcomes(spadl)
#' spadl <- label_actions_with_outcomes(spadl, outcomes)
#' plot_match_chains(spadl, match_id = "abc123")
#' plot_match_chains(spadl, match_id = "abc123", team_id = "team1")
#' }
plot_match_chains <- function(spadl_with_chains, target_match_id,
                               target_team_id = NULL,
                               highlight_goals = TRUE,
                               background = c("white", "green")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for chain visualization.")
  }

  background <- match.arg(background)

  match_data <- spadl_with_chains[spadl_with_chains$match_id == target_match_id, ]
  if (nrow(match_data) == 0) {
    cli::cli_abort("No actions found for match_id={target_match_id}")
  }

  if (!is.null(target_team_id)) {
    match_data <- match_data[match_data$team_id == target_team_id, ]
    if (nrow(match_data) == 0) {
      cli::cli_abort("No actions found for team_id={target_team_id}")
    }
  }

  # Create chain group key for ggplot grouping
  match_data$chain_key <- paste(match_data$match_id, match_data$chain_id,
                                match_data$team_id, sep = "_")

  # Identify goal chains
  has_outcome <- "chain_outcome" %in% names(match_data)
  if (has_outcome && highlight_goals) {
    match_data$is_goal_chain <- match_data$chain_outcome == "goal"
  } else {
    match_data$is_goal_chain <- FALSE
  }

  p <- draw_pitch(background)

  # Draw non-goal chains as semi-transparent paths
  non_goal <- match_data[!match_data$is_goal_chain, ]
  if (nrow(non_goal) > 0) {
    p <- p + ggplot2::geom_path(
      data = non_goal,
      ggplot2::aes(x = .data$start_x, y = .data$start_y,
                   group = .data$chain_key),
      color = "#3498db", alpha = 0.1, linewidth = 0.4
    )
  }

  # Highlight goal chains
  goal_chains <- match_data[match_data$is_goal_chain, ]
  if (nrow(goal_chains) > 0) {
    p <- p + ggplot2::geom_path(
      data = goal_chains,
      ggplot2::aes(x = .data$start_x, y = .data$start_y,
                   group = .data$chain_key),
      color = "#e74c3c", alpha = 0.8, linewidth = 1.2
    )
  }

  subtitle_parts <- target_match_id
  if (!is.null(target_team_id)) {
    subtitle_parts <- paste0(subtitle_parts, " | ", target_team_id)
  }
  n_chains <- length(unique(match_data$chain_key))
  n_goals <- length(unique(goal_chains$chain_key))

  p <- p + ggplot2::labs(
    title = "Possession Chains",
    subtitle = paste0(subtitle_parts, " | ",
                      n_chains, " chains",
                      if (n_goals > 0) paste0(" (", n_goals, " goals)") else "")
  )

  p
}

library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(patchwork)

#' Visualize a Model Dyad Comparison
#'
#' @export
plot_dyad_comparison <- function(dyad_id_to_plot, dyads, states, registry) {

    # 1. Extract Dyad Row
    dyad_row <- dyads %>% filter(dyad_id == dyad_id_to_plot)
    if (nrow(dyad_row) == 0) stop("Dyad ID not found in matrix.")

    ego_id <- dyad_row$ego_id[1]
    alter_id <- dyad_row$alter_id[1]

    # 2. Helper to extract node/edge data and build an igraph object
    build_model_graph <- function(model_id) {
        # Extract states for this model
        m_states <- if(is.data.frame(states)) {
            states %>% filter(model_id == !!model_id)
        } else {
            do.call(rbind, lapply(states, as.data.frame)) %>% filter(model_id == !!model_id)
        }

        # Merge with registry to get details
        m_full <- m_states %>%
            left_join(registry, by = "comp_id")

        # Build Nodes (X = timing, Y = stacked based on timing)
        nodes <- m_full %>%
            filter(type == "node", status %in% c("present", "causal")) %>%
            select(name = source, timing) %>%
            arrange(timing, name) %>%
            group_by(timing) %>%
            mutate(
                x = timing,
                # Center the Y coordinates so the DAG is vertically balanced
                y = seq(-length(name)/2, length(name)/2, length.out = length(name))
            ) %>%
            ungroup()

        # Build Edges
        edges <- m_full %>%
            filter(type == "edge", status %in% c("causal", "non-causal", "unknown")) %>%
            select(from = source, to = target, status)

        # Only keep edges where both nodes are present
        edges <- edges %>%
            filter(from %in% nodes$name & to %in% nodes$name)

        # Create igraph object and attach coordinates
        g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
        return(g)
    }

    # 3. Helper to plot a single graph cleanly
    plot_single_dag <- function(g, title) {
        # Extract coordinates attached to the igraph object
        layout_matrix <- create_layout(g, layout = "manual",
                                       x = V(g)$x, y = V(g)$y)

        ggraph(layout_matrix) +
            # Causal edges: Solid line, with arrow
            geom_edge_link(aes(filter = status == "causal"),
                           arrow = arrow(length = unit(3, 'mm'), type = "closed"),
                           end_cap = circle(6, 'mm'),
                           edge_colour = "black", edge_width = 0.8) +

            # Non-causal edges: Dashed line, faded, NO arrow
            geom_edge_link(aes(filter = status == "non-causal"),
                           linetype = "dashed", alpha = 0.4,
                           edge_colour = "gray50", edge_width = 0.5) +

            # Unknown edges: Dotted line, with a "?" label in the middle
            geom_edge_link(aes(filter = status == "unknown", label = "?"),
                           linetype = "dotted", edge_colour = "blue", edge_width = 0.8,
                           angle_calc = 'along', label_dodge = unit(2, 'mm')) +

            # Nodes
            geom_node_point(size = 10, color = "lightblue") +
            geom_node_text(aes(label = name), fontface = "bold", size = 4) +

            theme_void() +
            labs(title = title) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
    }

    # 4. Generate the two plots
    p_ego <- plot_single_dag(build_model_graph(ego_id), paste("Ego:", ego_id))
    p_alter <- plot_single_dag(build_model_graph(alter_id), paste("Alter:", alter_id))

    # 5. Build the Metadata Text Panel
    # Safely handle basic vs. full mode columns
    mas_ego_text <- if("mas_ego" %in% names(dyad_row)) dyad_row$mas_ego else "N/A"
    mas_alter_text <- if("mas_alter" %in% names(dyad_row)) dyad_row$mas_alter else "N/A"
    mas_compat <- if("mas_compatible" %in% names(dyad_row)) dyad_row$mas_compatible else "N/A"
    id_ego <- if("identified_ego" %in% names(dyad_row)) dyad_row$identified_ego else "N/A"
    id_alter <- if("identified_alter" %in% names(dyad_row)) dyad_row$identified_alter else "N/A"
    id_compat <- if("identified_compatible" %in% names(dyad_row)) dyad_row$identified_compatible else "N/A"

    meta_text <- sprintf(
        "Dyad Comparison: %s\n\nStructural Similarity: %.2f%%\nTiming Compatible: %s | Existence Conflict: %s | Repair Cost: %d\n\n--- Causal Metrics ---\nMAS Ego: %s (Identified: %s)\nMAS Alter: %s (Identified: %s)\nMAS Compatible: %s | Identified Compatible: %s",
        dyad_id_to_plot,
        dyad_row$similarity_rate * 100,
        dyad_row$timing_compatible, dyad_row$existence_conflict, dyad_row$repair_cost,
        mas_ego_text, id_ego,
        mas_alter_text, id_alter,
        mas_compat, id_compat
    )

    # Create a ggplot object containing just the text
    p_meta <- ggplot() +
        annotate("text", x = 0, y = 0, label = meta_text,
                 hjust = 0, vjust = 1, size = 4, family = "mono") +
        theme_void() +
        coord_cartesian(xlim = c(0, 10), ylim = c(-10, 0))

    # 6. Stitch it all together with patchwork
    # Ego and Alter side-by-side on top, metadata panel across the bottom
    final_plot <- (p_ego | p_alter) / p_meta +
        plot_layout(heights = c(3, 1))

    return(final_plot)
}
